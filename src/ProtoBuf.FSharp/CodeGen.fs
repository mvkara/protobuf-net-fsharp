module internal ProtoBuf.FSharp.CodeGen

open System
open FSharp.Reflection
open System.Collections.Concurrent
open System.Reflection
open System.Reflection.Emit
open MethodHelpers

type private TypeBuilder with
    member tb.DefineOpExplicit(src : Type, dst : Type) =
        let attr = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName ||| MethodAttributes.Static
        tb.DefineMethod("op_Explicit", attr, dst, [| src |])

    member tb.SetProtoContractAttribute(skipConstructor : bool) =
        let t = typeof<ProtoBuf.ProtoContractAttribute>
        CustomAttributeBuilder(
            t.GetConstructor [||], [||]
            , [| t.GetProperty "ImplicitFields" ; t.GetProperty "SkipConstructor" |]
            , [| box ProtoBuf.ImplicitFields.AllFields ; box skipConstructor |]
        ) |> tb.SetCustomAttribute

    member tb.DefineFieldForProtobuf(fi : PropertyInfo) =
        tb.DefineField(fi.Name, fi.PropertyType, FieldAttributes.Public) // Do something with name and attributes?


let private emitZeroValueOntoEvaluationStack (gen: ILGenerator) (getterType: MethodType) =
    match getterType with
    | MethodType.MethodInfo mi ->
        gen.EmitCall(OpCodes.Call, mi, null)
    | MethodType.PropertyInfo pi ->
        gen.EmitCall(OpCodes.Call, pi.GetMethod, null)
    | MethodType.FieldInfo fi ->
        gen.Emit(OpCodes.Ldsfld, fi)
    | MethodType.NewArray elementType ->
        gen.Emit(OpCodes.Ldc_I4_0) // Push length onto the stack.
        gen.Emit(OpCodes.Newarr, elementType) // Initialise array with length.

let private emitStackTopZeroCheck (gen : ILGenerator) (topType : Type) =
    ZeroValues.getZeroValueMethodInfoOpt topType |> Option.iter (fun getValue ->
        let skip = gen.DefineLabel()
        gen.Emit(OpCodes.Dup)
        gen.Emit(OpCodes.Brtrue, skip)
        gen.Emit(OpCodes.Pop)
        emitZeroValueOntoEvaluationStack gen getValue
        gen.MarkLabel(skip)
    )

let private emitFieldAssignments (gen: ILGenerator) (zeroValuesPerField: ZeroValues.FieldWithZeroValueSetMethod[]) =
    for zeroValueField in zeroValuesPerField do
        if zeroValueField.FieldInfo.IsStatic then
            emitZeroValueOntoEvaluationStack gen zeroValueField.ZeroValueMethod
            gen.Emit(OpCodes.Stsfld, zeroValueField.FieldInfo) // Assign to field
        else
            gen.Emit(OpCodes.Dup)
            emitZeroValueOntoEvaluationStack gen zeroValueField.ZeroValueMethod
            gen.Emit(OpCodes.Stfld, zeroValueField.FieldInfo)

let private emitRecordDefault (gen: ILGenerator) (recordType: Type) =
    for pi in FSharpType.GetRecordFields(recordType, true) do
        let propertyType = pi.PropertyType

        match ZeroValues.getZeroValueMethodInfoOpt propertyType with
        | Some getValueMethodInfo ->
            emitZeroValueOntoEvaluationStack gen getValueMethodInfo
        | _ when propertyType.IsValueType ->
            let cell = gen.DeclareLocal(propertyType)
            gen.Emit(OpCodes.Ldloca_S, cell)
            gen.Emit(OpCodes.Initobj, propertyType)
            gen.Emit(OpCodes.Ldloc, cell)
        | _ ->
            gen.Emit(OpCodes.Ldnull)

    let ctr = FSharpValue.PreComputeRecordConstructorInfo(recordType, true)
    gen.Emit(OpCodes.Newobj, ctr)

/// Emits a factory to create the object making sure all values are default assigned as expected for F# consumption (e.g. no nulls where not possible to define for common cases)
let private emitFactory (resultType : Type) (zeroValuesPerField: ZeroValues.FieldWithZeroValueSetMethod array) =
    let factoryMethod = DynamicMethod("factory_" + resultType.FullName, resultType, [| |], true)
    let gen = factoryMethod.GetILGenerator()

    match resultType.GetConstructor Array.empty with
    | null when FSharpType.IsRecord (resultType, true) -> // Is an F# record with a F# constructor.
        emitRecordDefault gen resultType
    | null -> // Is a type that isn't a record with no parameterless constructor. NOTE: This is significantly slower for deserialisation than alternative pathways.
        gen.Emit(OpCodes.Ldtoken, resultType)
        gen.EmitCall(OpCodes.Call, MethodHelpers.getMethodInfo <@ Runtime.Serialization.FormatterServices.GetUninitializedObject @> [||], null)
        emitFieldAssignments gen zeroValuesPerField
    | ctr -> // Has a parameterless constructor
        gen.Emit(OpCodes.Newobj, ctr)
        emitFieldAssignments gen zeroValuesPerField

    gen.Emit(OpCodes.Ret)
    factoryMethod :> MethodInfo


let private surrogatePrefix = "ProtoBuf.FSharp.Surrogates.Generated"

let private surrogateTypeDeclaration (surrogateModule: ModuleBuilder) (targetType: Type) (useValueTypeSurrogate: bool) =
    let surrogateType =
        let name = sprintf "%s.%s" surrogatePrefix targetType.FullName
        let attr = TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
        if useValueTypeSurrogate
        then surrogateModule.DefineType(name, attr, typeof<ValueType>)
        else surrogateModule.DefineType(name, attr)

    let constructor =
        if surrogateType.IsValueType
        then ValueNone
        else surrogateType.DefineDefaultConstructor MethodAttributes.Public |> ValueSome

    struct (surrogateType, constructor)

/// Emits a record surrogate. Intended to be used to support value type records ONLY since Protobuf-net at time of writing does not support custom ValueTypes/Structs.
let private emitRecordSurrogate (surrogateModule: ModuleBuilder) (recordType: Type) (useValueTypeSurrogate: bool) =
    let struct (surrogateType, constructor) = surrogateTypeDeclaration surrogateModule recordType useValueTypeSurrogate
    surrogateType.SetProtoContractAttribute(useValueTypeSurrogate)

    let surrogateFields = [|
        for fi in FSharpType.GetRecordFields(recordType, true) ->
            struct (fi, surrogateType.DefineFieldForProtobuf fi)
    |]

    // Define op_Explicit methods that Protobuf calls to create recordType from surrogate.
    let conv = surrogateType.DefineOpExplicit(surrogateType, recordType)
    let gen = conv.GetILGenerator()
    for (recordField, surrogateField) in surrogateFields do
        gen.Emit((if surrogateType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
        gen.Emit(OpCodes.Ldfld, surrogateField)
        emitStackTopZeroCheck gen recordField.PropertyType
    gen.Emit(OpCodes.Newobj, FSharpValue.PreComputeRecordConstructorInfo(recordType, true))
    gen.Emit(OpCodes.Ret)

    // Define op_Explicit methods that Protobuf calls to create surrogate from recordType.
    let conv = surrogateType.DefineOpExplicit(recordType, surrogateType)
    let gen = conv.GetILGenerator()

    let cell = gen.DeclareLocal(surrogateType)
    match constructor with
    | ValueSome ctr ->
        gen.Emit(OpCodes.Newobj, ctr)
        gen.Emit(OpCodes.Stloc, cell)
    | ValueNone ->
        gen.Emit(OpCodes.Ldloca_S, cell)
        gen.Emit(OpCodes.Initobj, surrogateType)

    let argIsNull = gen.DefineLabel()
    if not recordType.IsValueType
    then
        gen.Emit(OpCodes.Ldarg_0)
        gen.Emit(OpCodes.Brfalse, argIsNull)

    for (recordField, surrogateField) in surrogateFields do
        gen.Emit((if surrogateType.IsValueType then OpCodes.Ldloca_S else OpCodes.Ldloc), cell)
        gen.Emit((if recordType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
        gen.Emit(OpCodes.Call, recordField.GetMethod)
        gen.Emit(OpCodes.Stfld, surrogateField)

    gen.MarkLabel(argIsNull)
    gen.Emit(OpCodes.Ldloc, cell)
    gen.Emit(OpCodes.Ret)

    surrogateType.CreateTypeInfo ()

let private emitGetUnionTag (gen : ILGenerator) (unionType: Type) =
    match FSharpValue.PreComputeUnionTagMemberInfo(unionType, true) with
    | :? PropertyInfo as tag ->
        gen.Emit((if unionType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
        gen.Emit(OpCodes.Call, tag.GetMethod)
    | :? MethodInfo as tag when tag.IsStatic ->
        gen.Emit(OpCodes.Call, tag)
    | smth -> failwithf "Unexpected tag member: %A" smth

let private emitValueUnionSurrogate (surrogateModule: ModuleBuilder) (unionType: Type) (useValueTypeSurrogate: bool) =
    let struct (surrogateType, constructor) = surrogateTypeDeclaration surrogateModule unionType useValueTypeSurrogate
    surrogateType.SetProtoContractAttribute(useValueTypeSurrogate)

    let cases = [|
        for caseInfo in FSharpType.GetUnionCases(unionType, true) ->
            struct (caseInfo, [|
                for fi in caseInfo.GetFields() ->
                    struct (fi, surrogateType.DefineFieldForProtobuf fi)
            |])
    |]

    let surrogateTagField =
        let rec chooseName name =
            match unionType.GetMember(name, BindingFlags.Public ||| BindingFlags.NonPublic) with
            | [||] -> name
            | _ -> chooseName ("_" + name)
        surrogateType.DefineField(chooseName "__tag", typeof<int>, FieldAttributes.Public)

    // Define op_Explicit methods that Protobuf calls to create unionType from surrogate.
    begin
        let conv = surrogateType.DefineOpExplicit(surrogateType, unionType)
        let gen = conv.GetILGenerator()

        let jumpTable = Array.init (Array.length cases) (ignore >> gen.DefineLabel)

        gen.Emit((if surrogateType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
        gen.Emit(OpCodes.Ldfld, surrogateTagField)
        gen.Emit(OpCodes.Switch, jumpTable)

        for (caseInfo, surrogateFields) in cases do
            gen.MarkLabel(jumpTable.[ caseInfo.Tag ])
            for (originField, surrogateField) in surrogateFields do
                gen.Emit((if surrogateType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
                gen.Emit(OpCodes.Ldfld, surrogateField)
                emitStackTopZeroCheck gen originField.PropertyType
            gen.Emit(OpCodes.Call, FSharpValue.PreComputeUnionConstructorInfo(caseInfo, true))
            gen.Emit(OpCodes.Ret)
    end

    // Define op_Explicit methods that Protobuf calls to create surrogate from unionType.
    begin
        let conv = surrogateType.DefineOpExplicit(unionType, surrogateType)
        let gen = conv.GetILGenerator()

        let resultCell = gen.DeclareLocal(surrogateType)
        match constructor with
        | ValueSome ctr ->
            gen.Emit(OpCodes.Newobj, ctr)
            gen.Emit(OpCodes.Stloc, resultCell)
        | ValueNone ->
            gen.Emit(OpCodes.Ldloca_S, resultCell)
            gen.Emit(OpCodes.Initobj, surrogateType)

        let jumpTable = Array.init (Array.length cases) (ignore >> gen.DefineLabel)

        emitGetUnionTag gen unionType
        gen.Emit(OpCodes.Switch, jumpTable)

        for (caseInfo, surrogateFields) in cases do
            gen.MarkLabel(jumpTable.[ caseInfo.Tag ])
            gen.Emit((if surrogateType.IsValueType then OpCodes.Ldloca_S else OpCodes.Ldloc), resultCell)
            for (originField, surrogateField) in surrogateFields do
                gen.Emit(OpCodes.Dup)
                gen.Emit(OpCodes.Ldarga_S, 0)
                gen.Emit(OpCodes.Call, originField.GetMethod)
                gen.Emit(OpCodes.Stfld, surrogateField)
            gen.Emit(OpCodes.Ldc_I4, caseInfo.Tag)
            gen.Emit(OpCodes.Stfld, surrogateTagField)
            gen.Emit(OpCodes.Ldloc, resultCell)
            gen.Emit(OpCodes.Ret)
    end

    surrogateType.CreateTypeInfo ()

let relevantUnionSubtypes (unionType: Type) = seq {
    for tt in unionType.GetNestedTypes(BindingFlags.Public ||| BindingFlags.NonPublic) do
        let subtype =
            if unionType.IsGenericType && tt.IsGenericTypeDefinition
                then tt.MakeGenericType(unionType.GetGenericArguments())
                else tt
        if subtype.IsSubclassOf unionType then
            yield subtype
}

let private emitUnionSurrogate (surrogateModule: ModuleBuilder) (unionType: Type) =
    let surrogateType =
        let name = sprintf "%s.%s" surrogatePrefix unionType.FullName
        let attr = TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
        surrogateModule.DefineType(name, attr, typeof<ValueType>)
    surrogateType.SetProtoContractAttribute(false)

    let surrogateTagField =
        surrogateType.DefineField("Tag", typeof<int>, FieldAttributes.Public)

    let cases = [|
        for caseInfo in FSharpType.GetUnionCases(unionType, true) ->
            let caseData =
                match caseInfo.GetFields() with
                | [||] -> ValueNone
                | caseFields ->
                    let subtype =
                        let attr = TypeAttributes.Public ||| TypeAttributes.NestedPublic ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
                        surrogateType.DefineNestedType("Case" + caseInfo.Name, attr)
                    subtype.SetProtoContractAttribute(false)

                    let fields = [| for fi in caseFields -> struct (fi, subtype.DefineFieldForProtobuf(fi)) |]

                    subtype.DefineDefaultConstructor MethodAttributes.Public |> ignore
                    let constructor =
                        let paramType = if unionType.IsValueType then unionType.MakeByRefType() else unionType
                        let ctr = subtype.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| paramType |])
                        let gen = ctr.GetILGenerator()
                        gen.Emit(OpCodes.Ldarg_0)
                        gen.Emit(OpCodes.Call, typeof<obj>.GetConstructor [||])
                        for (originField, surrogateField) in fields do
                            gen.Emit(OpCodes.Ldarg_0)
                            gen.Emit(OpCodes.Ldarg_1)
                            gen.Emit(OpCodes.Call, originField.GetMethod)
                            gen.Emit(OpCodes.Stfld, surrogateField)
                        gen.Emit(OpCodes.Ret)
                        ctr

                    let extractMethod =
                        let extract = subtype.DefineMethod("Extract", MethodAttributes.Public, unionType, [| |])
                        let gen = extract.GetILGenerator()
                        for (_, surrogateField) in fields do
                            gen.Emit(OpCodes.Ldarg_0)
                            gen.Emit(OpCodes.Ldfld, surrogateField)
                            emitStackTopZeroCheck gen surrogateField.FieldType
                        gen.Emit(OpCodes.Call, FSharpValue.PreComputeUnionConstructorInfo(caseInfo, true))
                        gen.Emit(OpCodes.Ret)
                        extract

                    let caseDataField =
                        surrogateType.DefineField("Data" + caseInfo.Name, subtype, FieldAttributes.Public)

                    subtype.CreateTypeInfo() |> ignore
                    struct (caseDataField, constructor, extractMethod) |> ValueSome
            struct (caseInfo, caseData)
    |]

    // Define op_Explicit methods that Protobuf calls to create unionType from surrogate.
    let fromSurrogate =
        let conv = surrogateType.DefineOpExplicit(surrogateType, unionType)
        let gen = conv.GetILGenerator()

        let jumpTable = Array.init (Array.length cases) (ignore >> gen.DefineLabel)
        gen.Emit((if surrogateType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
        gen.Emit(OpCodes.Ldfld, surrogateTagField)
        gen.Emit(OpCodes.Switch, jumpTable)

        for (caseInfo, caseStructure) in cases do
            gen.MarkLabel(jumpTable.[ caseInfo.Tag ])
            match caseStructure with
            | ValueSome struct (caseDataField, _, extractMethod) ->
                gen.Emit(OpCodes.Ldarga_S, 0)
                gen.Emit(OpCodes.Ldfld, caseDataField)
                gen.Emit(OpCodes.Call, extractMethod)
            | ValueNone ->
                gen.Emit(OpCodes.Call, FSharpValue.PreComputeUnionConstructorInfo(caseInfo, true))
            gen.Emit(OpCodes.Ret)
        conv

    // Define op_Explicit methods that Protobuf calls to create surrogate from unionType.
    let toSurrogate =
        let conv = surrogateType.DefineOpExplicit(unionType, surrogateType)
        let gen = conv.GetILGenerator()

        let resultCell = gen.DeclareLocal(surrogateType)
        gen.Emit(OpCodes.Ldloca_S, resultCell)
        gen.Emit(OpCodes.Initobj, surrogateType)

        let endLabel = gen.DefineLabel()
        if not unionType.IsValueType then
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Brfalse, endLabel)

        let jumpTable = Array.init (Array.length cases) (ignore >> gen.DefineLabel)
        emitGetUnionTag gen unionType
        gen.Emit(OpCodes.Switch, jumpTable)
        gen.Emit(OpCodes.Br, endLabel)

        for (caseInfo, caseStructure) in cases do
            gen.MarkLabel(jumpTable.[ caseInfo.Tag ])
            caseStructure |> ValueOption.iter (fun struct (caseDataField, caseConstructor, _) ->
                gen.Emit(OpCodes.Ldloca_S, resultCell)
                gen.Emit((if unionType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
                gen.Emit(OpCodes.Newobj, caseConstructor)
                gen.Emit(OpCodes.Stfld, caseDataField)
            )
            gen.Emit(OpCodes.Ldloca_S, resultCell)
            gen.Emit(OpCodes.Ldc_I4, caseInfo.Tag)
            gen.Emit(OpCodes.Stfld, surrogateTagField)
            gen.Emit(OpCodes.Br, endLabel)

        gen.MarkLabel(endLabel)
        gen.Emit(OpCodes.Ldloc, resultCell)
        gen.Emit(OpCodes.Ret)
        conv

    for subtype in relevantUnionSubtypes unionType do
        surrogateType
            .DefineOpExplicit(surrogateType, subtype)
            .GetILGenerator()
            .Emit(OpCodes.Jmp, fromSurrogate)

        surrogateType
            .DefineOpExplicit(subtype, surrogateType)
            .GetILGenerator()
            .Emit(OpCodes.Jmp, toSurrogate)

    surrogateType.CreateTypeInfo ()


[<RequireQualifiedAccess>]
type internal TypeConstructionStrategy =
    | NoCustomConstructor // Uses default Protobuf-net behaviour
    | CustomFactoryMethod of factoryMethod : MethodInfo
    | ObjectSurrogate of surrogateType : TypeInfo

let private surrogateAssembly = AssemblyBuilder.DefineDynamicAssembly(AssemblyName("SurrogateAssembly"), AssemblyBuilderAccess.Run)
let private surrogateModule = surrogateAssembly.DefineDynamicModule "SurrogateModule"

let private createConstructionMethod (typeToAdd : Type) zeroValuesForFields =
    if typeToAdd.IsValueType && FSharpType.IsRecord (typeToAdd, true) then
        emitRecordSurrogate surrogateModule typeToAdd typeToAdd.IsValueType |> TypeConstructionStrategy.ObjectSurrogate
    elif typeToAdd.IsValueType && FSharpType.IsUnion (typeToAdd, true) then
        emitValueUnionSurrogate surrogateModule typeToAdd true |> TypeConstructionStrategy.ObjectSurrogate
    else emitFactory typeToAdd zeroValuesForFields |> TypeConstructionStrategy.CustomFactoryMethod


let private metaInfoTypeCache = ConcurrentDictionary<Type, Lazy<TypeConstructionStrategy>>()

let getTypeConstructionMethod (typeToAdd : Type) (fields : FieldInfo[]) =
    match ZeroValues.calculateApplicableFields fields with
    | [| |] -> TypeConstructionStrategy.NoCustomConstructor
    | zeroValuesForFields ->
        metaInfoTypeCache.GetOrAdd(typeToAdd, fun _ -> lazy createConstructionMethod typeToAdd zeroValuesForFields).Value


let private unionSurrogateCache = ConcurrentDictionary<Type, Lazy<TypeInfo>>()

let getUnionSurrogate (typeToAdd : Type) =
    unionSurrogateCache.GetOrAdd(typeToAdd, fun _ -> lazy emitUnionSurrogate surrogateModule typeToAdd).Value
