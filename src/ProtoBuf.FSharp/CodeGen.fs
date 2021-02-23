module private ProtoBuf.FSharp.CodeGen

open System
open FSharp.Reflection
open System.Collections.Concurrent
open System.Reflection
open System.Reflection.Emit


let private emitFieldAssignments (gen : ILGenerator, assigns : struct (FieldInfo * MethodInfo)[]) =
    for (fi, getValue) in assigns do
        if fi.IsStatic then
            gen.EmitCall(OpCodes.Call, getValue, null)
            gen.Emit(OpCodes.Stsfld, fi)
        else
            gen.Emit(OpCodes.Dup)
            gen.EmitCall(OpCodes.Call, getValue, null)
            gen.Emit(OpCodes.Stfld, fi)


let private emitFactory (resultType : Type, assigns) =
    let factoryMethod = DynamicMethod("factory_" + resultType.FullName, resultType, [| |], true)
    let gen = factoryMethod.GetILGenerator()

    match resultType.GetConstructor [| |] with
    | null when FSharpType.IsRecord (resultType, true) ->
        for pi in FSharpType.GetRecordFields (resultType, true) do
            let tp = pi.PropertyType
            match ZeroValues.calculateIfApplicable tp with
            | Some getValue -> gen.EmitCall(OpCodes.Call, getValue, null)
            | _ when tp.IsValueType ->
                let cell = gen.DeclareLocal(tp)
                gen.Emit(OpCodes.Ldloca_S, cell)
            | _ -> gen.Emit(OpCodes.Ldnull)
        let ctr = FSharpValue.PreComputeRecordConstructorInfo (resultType, true)
        gen.Emit (OpCodes.Newobj, ctr)
    | null ->
        gen.Emit(OpCodes.Ldtoken, resultType)
        gen.EmitCall(OpCodes.Call, MethodHelpers.getMethodInfo <@ Runtime.Serialization.FormatterServices.GetUninitializedObject @> [||], null)
        emitFieldAssignments (gen, assigns)
    | ctr ->
        gen.Emit (OpCodes.Newobj, ctr)
        emitFieldAssignments (gen, assigns)

    gen.Emit(OpCodes.Ret)
    factoryMethod :> MethodInfo


let private emitRecordSurrogate (surrogateModule : ModuleBuilder, recordType : Type, useValueTypeSurrogate : bool) =
    let surrogateType =
        let name = recordType.Name + "Surrogate"
        let attr = TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
        if useValueTypeSurrogate then
            surrogateModule.DefineType (name, attr, typeof<ValueType>)
        else
            surrogateModule.DefineType (name, attr)

    let surrogateFields = [|
        for fi in FSharpType.GetRecordFields (recordType, true) ->
            struct (fi, surrogateType.DefineField (fi.Name, fi.PropertyType, FieldAttributes.Public))
    |]

    let constructor =
        let ctr = surrogateType.DefineConstructor (MethodAttributes.Public, CallingConventions.Standard, [| |])
        let gen = ctr.GetILGenerator ()
        for (field, surrogateField) in surrogateFields do
            ZeroValues.calculateIfApplicable field.PropertyType |> Option.iter (fun getValue ->
                gen.Emit((if surrogateType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
                gen.EmitCall(OpCodes.Call, getValue, null)
                gen.Emit(OpCodes.Stfld, surrogateField)
            )
        gen.Emit(OpCodes.Ret)
        ctr

    let attr = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName ||| MethodAttributes.Static
    do
        let conv = surrogateType.DefineMethod ("op_Implicit", attr, recordType, [| surrogateType |])
        let gen = conv.GetILGenerator ()
        let ctr = FSharpValue.PreComputeRecordConstructorInfo (recordType, true)
        for (_, surrogateField) in surrogateFields do
            gen.Emit((if surrogateType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
            gen.Emit(OpCodes.Ldfld, surrogateField)
        gen.Emit(OpCodes.Newobj, ctr)
        gen.Emit(OpCodes.Ret)

    do
        let conv = surrogateType.DefineMethod ("op_Implicit", attr, surrogateType, [| recordType |])
        let gen = conv.GetILGenerator ()
        gen.Emit(OpCodes.Newobj, constructor)

        let toEnd = gen.DefineLabel()
        if not recordType.IsValueType then
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


[<Struct; RequireQualifiedAccess>]
type MetaInfoType =
    | JustFields
    | FieldsAndFactory of factoryMethod : MethodInfo
    | Surrogate of surrogateType : TypeInfo


let private surrogateAssembly = AssemblyBuilder.DefineDynamicAssembly (AssemblyName("SurrogateAssembly"), AssemblyBuilderAccess.Run)
let private surrogateModule = surrogateAssembly.DefineDynamicModule "SurrogateModule"

let private metaInfoTypeCache = ConcurrentDictionary<Type, MetaInfoType> ()

let getMetaInfoType (typeToAdd : Type, fields : FieldInfo[]) =
    let assigns = ZeroValues.calculateApplicableFields fields
    if Array.isEmpty assigns then
        MetaInfoType.JustFields
    else
        metaInfoTypeCache.GetOrAdd (typeToAdd, fun _ ->
            if typeToAdd.IsValueType && FSharpType.IsRecord (typeToAdd, true) then
                emitRecordSurrogate (surrogateModule, typeToAdd, typeToAdd.IsValueType) |> MetaInfoType.Surrogate
            else
                emitFactory (typeToAdd, assigns) |> MetaInfoType.FieldsAndFactory
        )
