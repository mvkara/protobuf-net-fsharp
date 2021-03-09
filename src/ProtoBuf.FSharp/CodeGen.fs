module internal ProtoBuf.FSharp.CodeGen

open System
open FSharp.Reflection
open System.Collections.Concurrent
open System.Reflection
open System.Reflection.Emit
open MethodHelpers


let inline emitZeroValueOntoEvaluationStack (gen: ILGenerator) (getterType: MethodType) = 
    match getterType with
    | MethodType.MethodInfo mi  ->
        gen.EmitCall(OpCodes.Call, mi, null) 
    | MethodType.PropertyInfo pi -> 
        gen.EmitCall(OpCodes.Call, pi.GetMethod, null) 
    | MethodType.FieldInfo fi ->
        gen.Emit(OpCodes.Ldsfld, fi)
    | MethodType.NewArray t -> 
        gen.Emit(OpCodes.Ldc_I4_0) // Push length onto the stack.
        gen.Emit(OpCodes.Newarr, t) // Initialise array with length.

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

/// Emits a record surrogate. Intended to be used to support value type records ONLY since Protobuf-net at time of writing does not support custom ValueTypes/Structs.
let private emitRecordSurrogate (surrogateModule: ModuleBuilder) (recordType: Type) (useValueTypeSurrogate: bool) =
    let surrogateType =
        let name = sprintf "%s.Generated.%s" (nameof ProtoBuf.FSharp.Surrogates) recordType.FullName
        let attr = TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
        if useValueTypeSurrogate
        then surrogateModule.DefineType(name, attr, typeof<ValueType>)
        else surrogateModule.DefineType(name, attr)

    let surrogateFields = [|
        for fi in FSharpType.GetRecordFields(recordType, true) ->
            struct (fi, surrogateType.DefineField(fi.Name, fi.PropertyType, FieldAttributes.Public))
    |]

    let constructor =
        let ctr = surrogateType.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| |])
        let gen = ctr.GetILGenerator()

        if not surrogateType.IsValueType
        then
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Call, typeof<obj>.GetConstructor [||])

        for (field, surrogateField) in surrogateFields do
            ZeroValues.getZeroValueMethodInfoOpt field.PropertyType
            |> Option.iter (fun getValue ->
                gen.Emit((if surrogateType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
                emitZeroValueOntoEvaluationStack gen getValue
                gen.Emit(OpCodes.Stfld, surrogateField))

        gen.Emit(OpCodes.Ret)
        ctr

    let attr = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName ||| MethodAttributes.Static

    // Define op_Implicit methods that Protobuf calls to create recordType from surrogate.
    let conv = surrogateType.DefineMethod("op_Implicit", attr, recordType, [| surrogateType |])
    let gen = conv.GetILGenerator()
    let ctr = FSharpValue.PreComputeRecordConstructorInfo(recordType, true)
    for (_, surrogateField) in surrogateFields do
        gen.Emit((if surrogateType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
        gen.Emit(OpCodes.Ldfld, surrogateField)
    gen.Emit(OpCodes.Newobj, ctr)
    gen.Emit(OpCodes.Ret)

    // Define op_Implicit methods that Protobuf calls to create surrogate from recordType.
    let conv = surrogateType.DefineMethod("op_Implicit", attr, surrogateType, [| recordType |])
    let gen = conv.GetILGenerator()
    gen.Emit(OpCodes.Newobj, constructor)

    let toEnd = gen.DefineLabel()
    if not recordType.IsValueType
    then
        gen.Emit(OpCodes.Ldarg_0)
        gen.Emit(OpCodes.Brfalse, toEnd)

    let cell = gen.DeclareLocal(surrogateType)
    gen.Emit(OpCodes.Stloc, cell)
    for (recordField, surrogateField) in surrogateFields do
        gen.Emit((if surrogateType.IsValueType then OpCodes.Ldloca_S else OpCodes.Ldloc), cell)
        gen.Emit((if recordType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
        gen.Emit(OpCodes.Call, recordField.GetMethod)
        gen.Emit(OpCodes.Stfld, surrogateField)
    gen.Emit(OpCodes.Ldloc, cell)

    gen.MarkLabel(toEnd)
    gen.Emit(OpCodes.Ret)

    surrogateType.CreateTypeInfo ()

[<RequireQualifiedAccess>]
type internal TypeConstructionStrategy =
    | NoCustomConstructor // Uses default Protobuf-net behaviour
    | CustomFactoryMethod of factoryMethod : MethodInfo
    | ObjectSurrogate of surrogateType : TypeInfo

let private surrogateAssembly = AssemblyBuilder.DefineDynamicAssembly(AssemblyName("SurrogateAssembly"), AssemblyBuilderAccess.Run)
let private surrogateModule = surrogateAssembly.DefineDynamicModule "SurrogateModule"
let private metaInfoTypeCache = ConcurrentDictionary<Type, TypeConstructionStrategy>()

let getTypeConstructionMethod (typeToAdd : Type) (fields : FieldInfo[]) =
    let zeroValuesForFields = ZeroValues.calculateApplicableFields fields
    if Array.isEmpty zeroValuesForFields
    then TypeConstructionStrategy.NoCustomConstructor
    else
        metaInfoTypeCache.GetOrAdd(typeToAdd, fun _ ->
            if typeToAdd.IsValueType && FSharpType.IsRecord (typeToAdd, true)
            then emitRecordSurrogate surrogateModule typeToAdd typeToAdd.IsValueType |> TypeConstructionStrategy.ObjectSurrogate
            else emitFactory typeToAdd zeroValuesForFields |> TypeConstructionStrategy.CustomFactoryMethod
        )
