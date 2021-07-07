namespace ProtoBuf.FSharp

open ProtoBuf
open ProtoBuf.Meta
open FSharp.Reflection
open System
open System.Reflection
open System.IO

module Serialiser =

    /// The magic number where if a union type has more than the above cases it simply is a tagged instance of the parent type.
    /// Otherwise for this number and below even non-empty unions get their own inner class prefixed with "_".
    let [<Literal>] private CasesCountWhereNoFieldCasesGenerateType = 3

    /// Allows users to register option types in advance. You can specify a custom suffix name for the Protobuf wrapper type generated.
    /// This only needs to be called directly if your type either is not already a field in another type previously registered (e.g. a record or union)
    /// and/or your not happy with the default type name in case of naming clashes.
    /// By default if None is provided for the customTypeSuffix parameter for example with Option<string> the protobuf message will be an "OptionalString".
    /// If the model is already registered (explictly or implicitly via another registration) AND/OR the type passed in is not an option type this will no-op.
    let registerOptionTypesIntoModel (optionType: Type) customTypeSuffix (model: RuntimeTypeModel) =
        if optionType.IsGenericType && optionType.GetGenericTypeDefinition() = typedefof<Option<_>> then
            let definedTypes = seq {
                for m in model.GetTypes() do
                let m = m :?> MetaType
                yield m.Type
            }

            if  definedTypes |> Seq.contains optionType |> not
            then
                let surrogateType = typedefof<Surrogates.Optional<_>>.MakeGenericType(optionType.GetGenericArguments())
                let surrogateModelType = model.Add(surrogateType, false)
                surrogateModelType.Name <- "Optional" + (customTypeSuffix |> Option.defaultValue (optionType.GetGenericArguments().[0].Name))
                surrogateModelType.AddField(1, "HasValue") |> ignore
                surrogateModelType.AddField(2, "Item") |> ignore
                let mt = model.Add(optionType, false)
                mt.SetSurrogate(surrogateType)

    let private addFieldsToMetaType (metaType : MetaType) (fields : FieldInfo[]) =
        for (index, fieldInfo) in Seq.indexed fields do
            let fieldModel = metaType.AddField(1 + index, fieldInfo.Name)
            fieldModel.BackingMember <- fieldInfo
            fieldModel.OverwriteList <- true
            fieldModel.Name <- fieldInfo.Name.TrimStart('_').TrimEnd('@') // Still not perfect --- F# allows pretty wild names (some cause protobuf to fail)

    let private processFieldsAndCreateFieldSetters (typeToAdd: Type) (metaType: MetaType) (model : RuntimeTypeModel) =
        let fields = typeToAdd.GetFields(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.GetField)
        metaType.UseConstructor <- false
        match CodeGen.getTypeConstructionMethod typeToAdd fields with
        | CodeGen.TypeConstructionStrategy.ObjectSurrogate surrogateType ->
            metaType.SetSurrogate surrogateType
        | CodeGen.TypeConstructionStrategy.CustomFactoryMethod factoryMethod ->
            addFieldsToMetaType metaType fields
            metaType.SetFactory factoryMethod |> ignore
        | CodeGen.TypeConstructionStrategy.NoCustomConstructor ->
            addFieldsToMetaType metaType fields

        for field in fields do registerOptionTypesIntoModel field.FieldType None model

    let registerUnionRuntimeTypeIntoModel' (unionType: Type) (model: RuntimeTypeModel) =
        let metaType = model.Add(unionType, true)
        let surrogateType = CodeGen.getUnionSurrogate unionType
        metaType.SetSurrogate surrogateType
        for subtype in CodeGen.relevantUnionSubtypes unionType do
            model.Add(subtype, false).SetSurrogate(surrogateType)
        model

    let registerUnionRuntimeTypeIntoModel (unionType: Type) (model: RuntimeTypeModel) =
        let unionCaseData = FSharpType.GetUnionCases(unionType, true)

        // Register the supertype in all cases
        let mt = model.Add(unionType, true)
        mt.UseConstructor <- false
        processFieldsAndCreateFieldSetters unionType mt model |> ignore

        // If there are no fields in any properties then we can assume the F# compiler has compiled
        // the class in a non-flat fashion. Structs are still compiled in a flat way (F# 4.1+ struct DU's).
        // Note: Protobuf doesn't quite support custom factories of structs failing at the verification so we are not supporting this.
        let isReferenceMulticaseDuWithPayload = not (unionType.IsValueType || unionCaseData |> Seq.collect (fun x -> x.GetFields()) |> Seq.isEmpty)

        if isReferenceMulticaseDuWithPayload
        then
            if unionCaseData.Length > 1 // Otherwise all fields would already be populated in the union root type before this IF statement.
            then
                for ucd in unionCaseData do
                    let candidateTypes = unionType.GetNestedTypes(BindingFlags.Public ||| BindingFlags.NonPublic)

                    let typeToAddOpt =
                        candidateTypes
                        |> Seq.tryFind (fun x -> x.Name = ucd.Name || x.Name = "_" + ucd.Name) // For under 3 cases classes with no fields are private with a "_" prefix.
                        |> Option.map
                            (fun typeToAdd ->
                                // Handle generic typed unions
                                if unionType.IsGenericType && typeToAdd.IsGenericTypeDefinition
                                then typeToAdd.MakeGenericType(unionType.GetGenericArguments())
                                else typeToAdd)

                    let typeToAddOpt =
                        match typeToAddOpt with
                        | Some(t) -> Some t
                        | None ->
                            if ucd.GetFields().Length = 0 && unionCaseData.Length > CasesCountWhereNoFieldCasesGenerateType
                            then None // In this case the "Tag" field is used by the F# compiler rather than an instance test.
                            else
                                failwithf
                                    "Couldn't find expected type for union case [UnionType: %A, InnerCaseName: %s, UnionCaseInfo: %A, CandidateTypes: %A]"
                                    unionType.FullName ucd.Name ucd (candidateTypes |> Seq.map (fun x -> x.FullName))

                    // The union may be a supertype union with no values hence no subtype. Should use the supertype as appropriate and skip this case.
                    match typeToAddOpt with
                    | Some(typeToAdd) ->
                        let caseTypeModel = model.Add(typeToAdd, false)
                        let tag = 1000 + ucd.Tag
                        mt.AddSubType(tag, typeToAdd) |> ignore
                        caseTypeModel.UseConstructor <- false
                        caseTypeModel.Name <- ucd.Name
                        processFieldsAndCreateFieldSetters typeToAdd caseTypeModel model |> ignore
                    | None -> ()
        model

    let registerUnionIntoModel<'tunion> model = registerUnionRuntimeTypeIntoModel typeof<'tunion> model

    let registerRecordRuntimeTypeIntoModel (runtimeType: Type) (model: RuntimeTypeModel) =
        let metaType = model.Add(runtimeType, false)
        metaType.UseConstructor <- false
        processFieldsAndCreateFieldSetters runtimeType metaType model |> ignore
        model

    let registerRecordIntoModel<'t> (model: RuntimeTypeModel) = registerRecordRuntimeTypeIntoModel typeof<'t> model

    let registerRuntimeTypeIntoModel (runtimeType: Type) (model: RuntimeTypeModel) =
        if FSharpType.IsRecord (runtimeType, true)
        then registerRecordRuntimeTypeIntoModel runtimeType model
        elif FSharpType.IsUnion (runtimeType, true)
        then registerUnionRuntimeTypeIntoModel runtimeType model
        else
            model.Add(runtimeType, true) |> ignore
            model

    let registerTypeIntoModel<'t> (model: RuntimeTypeModel) = registerRuntimeTypeIntoModel typeof<'t> model

    let serialise (model: RuntimeTypeModel) (stream: Stream) (o: 't) = model.Serialize(stream, o)

    let deserialise<'t> (model: RuntimeTypeModel) (stream: Stream) = model.Deserialize(stream, null, typeof<'t>) :?> 't

    let defaultModel = RuntimeTypeModel.Default