namespace ProtoBuf.FSharp

open ProtoBuf
open ProtoBuf.Meta
open FSharp.Reflection
open System
open System.Reflection
open System.IO
open System.Collections.Concurrent
open Microsoft.FSharp.Quotations.Patterns
open System.Reflection.Emit
open System.Linq.Expressions

module private Surrogates =

    type [<CLIMutable>] Optional<'t> =
        { HasValue: bool
          Item: 't }
        static member op_Implicit (w: Optional<'t>) : 't option =
            if w.HasValue then Some w.Item else None
        static member op_Implicit (o: 't option) =
            match o with
            | Some(o) -> { Item = o; HasValue = true }
            | None -> { HasValue = false; Item = Unchecked.defaultof<_> }

module private MethodHelpers =

    /// Allows you to get the nameof a method in older F# versions
    let private nameOfQuotation methodQuotation =
        match methodQuotation with
        | Lambda(_, Call(_, mi, _))
        | Lambda(_, Lambda(_, Call(_, mi, _))) -> mi.DeclaringType, mi.Name
        | x -> failwithf "Not supported %A" x

    // NOTE: Resorting to Reflection.Emit for private readonly setters since F# types are immutable on the surface.
    // Expression.Assign for example has a check against setting readonly fields
    let getFieldInfoSetterDelegate<'t, 'tfield> (fi: FieldInfo) =
        let methodName = fi.ReflectedType.FullName + "set_" + fi.Name
        let setterMethod = DynamicMethod(methodName, null, [| typeof<'t>; typeof<'tfield> |], true)
        let gen = setterMethod.GetILGenerator()
        if fi.IsStatic
        then
            gen.Emit(OpCodes.Ldarg_1)
            gen.Emit(OpCodes.Stsfld, fi)
        else
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Ldarg_1)
            gen.Emit(OpCodes.Stfld, fi)
        gen.Emit(OpCodes.Ret)
        setterMethod.CreateDelegate(typeof<Action<'t, 'tfield>>) :?> Action<'t, 'tfield>

    let private bindingFlagsToUse = BindingFlags.NonPublic ||| BindingFlags.Static

    let getMethodInfo quotation (typeParameters: Type array) =
        let (declaringType, nameOfMethod) = nameOfQuotation quotation
        match typeParameters.Length with
        | 0 -> declaringType.GetMethod(nameOfMethod, bindingFlagsToUse)
        | _ -> declaringType.GetMethod(nameOfMethod, bindingFlagsToUse).MakeGenericMethod(typeParameters)

    let objectConstructor = ConcurrentDictionary<Type, Func<obj>>()

    /// The CLIMutable attribute provides a quick way to construct objects. We should use it IF possible over FormatterServices for performance.
    let constructObjectWithoutConstructor<'t>() =

        let constructorMethod (parameterlessConstructor: ConstructorInfo) =
            Expression.Lambda(typeof<Func<obj>>, Expression.Convert(Expression.New(parameterlessConstructor, [||]), typeof<obj>)).Compile() :?> Func<obj>

        let constructorFunction =
            objectConstructor.GetOrAdd(
                typeof<'t>,
                (fun (typeToConstruct: Type) ->
                    let parameterlessConstructorOpt = typeToConstruct.GetConstructors() |> Seq.tryFind (fun constructor -> constructor.GetParameters().Length = 0)
                    match parameterlessConstructorOpt with
                    | Some(constructor) -> constructorMethod constructor // The CLIMutable case. Significant performance can be found here.
                    | None -> Func<_>(fun () -> Runtime.Serialization.FormatterServices.GetUninitializedObject(typeToConstruct))))

        constructorFunction.Invoke() :?> 't

    let staticSetters = new ConcurrentDictionary<Type, Delegate>()
    let zeroValues = ConcurrentDictionary<Type, obj>()

/// Used to create callbacks that set zero values to support roundtrip serailisation identity for F# types. Something that Protobuf-net doesn't do at time of writing.
/// This is all about priming the type so that there isn't nulls when doing roundtrip of collections and strings due to Protobuf-net default behaviour.
type private GenericSetterFactory =

    static member GetEmptyFSharpList<'t>() : 't list = List.empty
    static member GetEmptyArray<'t>() : 't array = Array.empty
    static member GetEmptySet<'t when 't : comparison>() : Set<'t> = Set.empty
    static member GetEmptyMap<'t, 'tv when 't : comparison>() : Map<'t, 'tv> = Map.empty

    static member CalculateZeroValuesIfApplicable (fieldType: Type) =

        /// Creates the zero value for supported types that we know of.
        let createZeroValue() =
            if fieldType = typeof<string>
                then box String.Empty |> Some
            elif fieldType.IsGenericType && fieldType.GetGenericTypeDefinition() = typedefof<_ list>
                then
                    let methodInfo = MethodHelpers.getMethodInfo <@ GenericSetterFactory.GetEmptyFSharpList @> fieldType.GenericTypeArguments
                    methodInfo.Invoke(null, [||]) |> Some
            elif fieldType.IsGenericType && fieldType.GetGenericTypeDefinition() = typedefof<Set<_>>
                then
                    let methodInfo = MethodHelpers.getMethodInfo <@ GenericSetterFactory.GetEmptySet @> fieldType.GenericTypeArguments
                    methodInfo.Invoke(null, [||]) |> Some
            elif fieldType.IsGenericType && fieldType.GetGenericTypeDefinition() = typedefof<Map<_, _>>
                then
                    let methodInfo = MethodHelpers.getMethodInfo <@ GenericSetterFactory.GetEmptyMap @> fieldType.GenericTypeArguments
                    methodInfo.Invoke(null, [||]) |> Some
            elif fieldType.IsArray
                then
                    let methodInfo = MethodHelpers.getMethodInfo <@ GenericSetterFactory.GetEmptyArray @> [| fieldType.GetElementType() |]
                    methodInfo.Invoke(null, [||]) |> Some
            else None

        match MethodHelpers.zeroValues.TryGetValue(fieldType) with
        | (true, zeroValue) -> Some zeroValue
        | (false, _) ->
            match createZeroValue() with
            | Some(zeroValue) ->
                MethodHelpers.zeroValues.[fieldType] <- zeroValue
                Some zeroValue
            | None -> None

    static member GetSetterCallbackGeneric<'t, 'tfield> (fi: FieldInfo) (zeroValue: 'tfield) : Delegate =
        let d = MethodHelpers.getFieldInfoSetterDelegate<'t, 'tfield> fi
        ((Action<'t> (fun t -> d.Invoke(t, zeroValue))) :> Delegate) // Manual partial completion of the zeroValue

    static member GetSetterCallbackIfApplicableForFieldInfo (fi: FieldInfo) : Delegate option =
        let zeroValueOpt = GenericSetterFactory.CalculateZeroValuesIfApplicable fi.FieldType

        match zeroValueOpt with
        | Some(zeroValue) ->
            let m = MethodHelpers.getMethodInfo <@ GenericSetterFactory.GetSetterCallbackGeneric @> [||]
            let gm = m.MakeGenericMethod([| fi.DeclaringType; fi.FieldType |])
            let gmParemeters = [| box fi; box zeroValue |]
            gm.Invoke(null, gmParemeters) :?> Delegate |> Some
        | None -> None

    static member SetNewCreationSetterDelegate fieldType (d: Delegate) =
        MethodHelpers.staticSetters.TryAdd(fieldType, d) |> ignore

    // The final delegate.
    static member public CreateOrDefault<'t> () =
        let item = MethodHelpers.constructObjectWithoutConstructor<'t>()
        match MethodHelpers.staticSetters.TryGetValue(typeof<'t>) with
        | (true, action) -> (action :?> Action<'t>).Invoke(item)
        | (false, _) -> ()
        item

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

        let definedTypes = seq {
            for m in model.GetTypes() do
            let m = m :?> MetaType
            yield m.Type
        }

        if
            optionType.IsGenericType
            && optionType.GetGenericTypeDefinition() = typedefof<Option<_>>
            && definedTypes |> Seq.contains optionType |> not
        then
            let surrogateType = typedefof<Surrogates.Optional<_>>.MakeGenericType(optionType.GetGenericArguments())
            let surrogateModelType = model.Add(surrogateType, false)
            surrogateModelType.Name <- "Optional" + (customTypeSuffix |> Option.defaultValue (optionType.GetGenericArguments().[0].Name))
            surrogateModelType.AddField(1, "HasValue") |> ignore
            surrogateModelType.AddField(2, "Item") |> ignore
            let mt = model.Add(optionType, false)
            mt.SetSurrogate(surrogateType)

    let private processFieldsAndCreateFieldSetters (typeToAdd: Type) (metaType: MetaType) model =
        let fields = typeToAdd.GetFields(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.GetField)
        metaType.UseConstructor <- false
        let _, fieldSetterDelegates =
            fields
            |> Array.fold
                (fun (index, delegates) fieldInfo ->
                    //let mt = mt.Add(fieldInfo.Name)
                    let fieldModel = metaType.AddField(index, fieldInfo.Name)
                    fieldModel.BackingMember <- fieldInfo
                    fieldModel.OverwriteList <- true
                    fieldModel.Name <- fieldInfo.Name.Replace("@", "").Replace("_", "")

                    match GenericSetterFactory.GetSetterCallbackIfApplicableForFieldInfo fieldInfo with
                    | Some(d) -> (index+1, d :: delegates)
                    | None -> (index+1, delegates)
                    )
                (1, [])

        for field in fields do
            registerOptionTypesIntoModel field.FieldType None model

        if not fieldSetterDelegates.IsEmpty
        then
            let setterDelegate = Delegate.Combine(fieldSetterDelegates |> List.toArray)
            GenericSetterFactory.SetNewCreationSetterDelegate typeToAdd setterDelegate
            let factoryMethodInfo = MethodHelpers.getMethodInfo <@ GenericSetterFactory.CreateOrDefault @> [| typeToAdd |]
            metaType.SetFactory(factoryMethodInfo)
        else metaType

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
        if FSharpType.IsRecord runtimeType
        then registerRecordRuntimeTypeIntoModel runtimeType model
        elif FSharpType.IsUnion runtimeType
        then registerUnionRuntimeTypeIntoModel runtimeType model
        else
            model.Add(runtimeType, true) |> ignore
            model

    let registerTypeIntoModel<'t> (model: RuntimeTypeModel) = registerRuntimeTypeIntoModel typeof<'t> model

    let serialise (model: RuntimeTypeModel) (stream: Stream) (o: 't) = model.Serialize(stream, o)

    let deserialise<'t> (model: RuntimeTypeModel) (stream: Stream) = model.Deserialize(stream, null, typeof<'t>) :?> 't

    let defaultModel = RuntimeTypeModel.Default