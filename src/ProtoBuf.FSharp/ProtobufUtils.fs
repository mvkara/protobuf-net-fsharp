namespace ProtoBuf.FSharp

open ProtoBuf
open ProtoBuf.Meta
open FSharp.Reflection
open System
open System.Reflection
open System.IO

module Serialiser =
    let private registerSurrogate (tp : Type) (model : RuntimeTypeModel) =
        let surrogateType = CodeGen.getSurrogate tp
        match surrogateType.GetMethod("RegisterIntoModel") with
        | null -> ()
        | method -> method.Invoke(null, [| box model |]) |> ignore
        model.Add(tp, false).SetSurrogate surrogateType
        surrogateType

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
            if definedTypes |> Seq.contains optionType |> not
            then
                registerSurrogate optionType model |> ignore

    let private attemptToRegisterFieldType (fieldType: Type) (model: RuntimeTypeModel) =
        if fieldType.IsGenericType then
            let def = fieldType.GetGenericTypeDefinition()
            if def = typedefof<_ option> then
                registerOptionTypesIntoModel fieldType None model
            elif def = typedefof<_ list> then
                CollectionRegistration.registerCollectionWithReflectedTypeIntoModel model fieldType |> ignore

    let private processFieldsAndCreateFieldSetters (typeToAdd: Type) (model : RuntimeTypeModel) =
        let metaType = model.Add(typeToAdd, false)
        metaType.UseConstructor <- false

        let fields = typeToAdd.GetFields(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.GetField)
        for (index, fieldInfo) in Seq.indexed fields do
            let fieldModel = metaType.AddField(1 + index, fieldInfo.Name)
            fieldModel.BackingMember <- fieldInfo
            fieldModel.OverwriteList <- true
            fieldModel.Name <- fieldInfo.Name.TrimStart('_').TrimEnd('@') // Still not perfect --- F# allows pretty wild names (some cause protobuf to fail)

        let zeroValuesForFields = ZeroValues.calculateApplicableFields fields
        if Array.length zeroValuesForFields > 0 then
            metaType.SetFactory(CodeGen.getFactory typeToAdd zeroValuesForFields) |> ignore

        metaType

    let private registerUnionDirectly (unionType: Type) (model: RuntimeTypeModel) =
        let unionCaseData = FSharpType.GetUnionCases(unionType, true)

        // Register the supertype in all cases
        let mt = processFieldsAndCreateFieldSetters unionType model

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
                        let caseTypeModel = processFieldsAndCreateFieldSetters typeToAdd model
                        caseTypeModel.Name <- ucd.Name
                        let tag = 1000 + ucd.Tag
                        mt.AddSubType(tag, typeToAdd) |> ignore
                    | None -> ()

    let private internalRegister useSurrogateForReferenceUnions (runtimeType: Type) (model: RuntimeTypeModel) =
        match runtimeType with
        | recordType when FSharpType.IsRecord(recordType, true) ->
            let fields = FSharpType.GetRecordFields(recordType, true)
            if recordType.IsValueType && fields |> Array.exists (fun pi -> ZeroValues.isApplicableTo pi.PropertyType) then
                registerSurrogate recordType model |> ignore
            else
                processFieldsAndCreateFieldSetters recordType model |> ignore

            for field in fields do
                attemptToRegisterFieldType field.PropertyType model

        | unionType when FSharpType.IsUnion(unionType, true) ->
            if unionType.IsGenericType && unionType.GetGenericTypeDefinition() = typedefof<Option<_>> then
                registerOptionTypesIntoModel unionType None model
            elif unionType.IsValueType || useSurrogateForReferenceUnions then
                let surrogateType = registerSurrogate unionType model
                for subtype in CodeGen.relevantUnionSubtypes unionType do
                    model.Add(subtype, false).SetSurrogate(surrogateType)
            else
                registerUnionDirectly unionType model

            for caseInfo in FSharpType.GetUnionCases(unionType, true) do
                for field in caseInfo.GetFields() do
                    attemptToRegisterFieldType field.PropertyType model

        | _ ->
            model.Add(runtimeType, true) |> ignore


    let registerRuntimeTypeIntoModel (runtimeType: Type) (model: RuntimeTypeModel) =
        internalRegister false runtimeType model
        model

    let registerTypeIntoModel<'t> (model: RuntimeTypeModel) =
        registerRuntimeTypeIntoModel typeof<'t> model


    let registerUnionRuntimeTypeIntoModel (unionType: Type) (model: RuntimeTypeModel) =
        if FSharpType.IsUnion(unionType, true) then
            registerRuntimeTypeIntoModel unionType model
        else
            failwithf "registerUnionRuntimeTypeIntoModel: %A is not a union" unionType

    let registerUnionIntoModel<'tunion> model =
        registerUnionRuntimeTypeIntoModel typeof<'tunion> model


    let registerRecordRuntimeTypeIntoModel (recordType: Type) (model: RuntimeTypeModel) =
        if FSharpType.IsRecord(recordType, true) then
            registerRuntimeTypeIntoModel recordType model
        else
            failwithf "registerRecordRuntimeTypeIntoModel: %A is not a record" recordType

    let registerRecordIntoModel<'t> (model: RuntimeTypeModel) =
        registerRecordRuntimeTypeIntoModel typeof<'t> model


    let serialise (model: RuntimeTypeModel) (stream: Stream) (o: 't) = model.Serialize(stream, o)

    let deserialise<'t> (model: RuntimeTypeModel) (stream: Stream) = model.Deserialize(stream, null, typeof<'t>) :?> 't

    let defaultModel = RuntimeTypeModel.Default