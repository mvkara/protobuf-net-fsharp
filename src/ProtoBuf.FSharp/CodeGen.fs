module internal ProtoBuf.FSharp.CodeGen

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading
open System.Reflection
open System.Reflection.Emit
open FSharp.Reflection
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

type private FieldBuilder with
    member fb.SetProtoMemberAttribute(tag : int) =
        let t = typeof<ProtoBuf.ProtoMemberAttribute>
        CustomAttributeBuilder(t.GetConstructor [| typeof<int> |], [| box tag |]) |> fb.SetCustomAttribute


/// Hack to get default value of type 'tp' on top of evaluation stack:
/// create local variable of type tp, initialize it with initobj opcode and read it.
/// Used if 'tp' is value type (for reference types ldnull works ok)
let private emitDefaultValueViaCell (gen : ILGenerator) (tp : Type) =
    let cell = gen.DeclareLocal(tp)
    gen.Emit(OpCodes.Ldloca_S, cell)
    gen.Emit(OpCodes.Initobj, tp)
    gen.Emit(OpCodes.Ldloc, cell)

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

/// Checks if value on top of evaluation stack (should be of type 'topType') is null
/// and 'topType' has zero value defined. If so, replaces stack top with corresponding zero value.
/// If 'topType' is generic parameter then the check is performed in runtime by calling
/// ZeroValues.isApplicableTo and ZeroValues.getZeroValue<'t>.
let private emitStackTopZeroCheck (gen : ILGenerator) (topType : Type) =
    if topType.IsGenericParameter then
        let skipZeroCheck = gen.DefineLabel()
        gen.Emit(OpCodes.Ldtoken, topType) // in runtime that loads whatever type topType is substituted with
        gen.Emit(OpCodes.Call, MethodHelpers.getMethodInfo <@ ZeroValues.isApplicableTo @> [| |])
        gen.Emit(OpCodes.Brfalse, skipZeroCheck)
        gen.Emit(OpCodes.Dup)
        gen.Emit(OpCodes.Brtrue, skipZeroCheck)
        gen.Emit(OpCodes.Pop)
        gen.Emit(OpCodes.Call, MethodHelpers.getMethodInfo <@ ZeroValues.getZeroValue @> [| topType |])
        gen.MarkLabel(skipZeroCheck)
    else
        ZeroValues.getZeroValueMethodInfoOpt topType |> Option.iter (fun getValue ->
            let skipZeroCheck = gen.DefineLabel()
            gen.Emit(OpCodes.Dup)
            gen.Emit(OpCodes.Brtrue, skipZeroCheck)
            gen.Emit(OpCodes.Pop)
            emitZeroValueOntoEvaluationStack gen getValue
            gen.MarkLabel(skipZeroCheck)
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
            emitDefaultValueViaCell gen propertyType
        | _ ->
            gen.Emit(OpCodes.Ldnull)

    let ctr = FSharpValue.PreComputeRecordConstructorInfo(recordType, true)
    gen.Emit(OpCodes.Newobj, ctr)


let mutable private uniqueNameCounter = 0L


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


let private getGenericArgs (t : Type) =
    if t.IsGenericTypeDefinition then
        t.GetGenericArguments() |> ValueSome
    else ValueNone

let private defineGenericArgs (args : ValueOption<Type[]>) (tb : TypeBuilder) =
    args |> ValueOption.iter (fun args -> tb.DefineGenericParameters [| for arg in args -> arg.Name |] |> ignore)

let private substituteGenericArgs args (t : Type) =
    match args with
    | ValueNone -> t
    | ValueSome args -> t.MakeGenericType args

/// Adds to 'tb':
/// * Fields representing 'targetFields'
/// * Extract method to get 'targetType' value from surrogate
/// * Constructor, that creates 'tb' from 'targetType' (if targetType is value type then it's passed by reference)
let private emitSurrogateContent (tb : TypeBuilder) (targetType : Type) (targetFields : PropertyInfo[]) (targetGenerate : MethodBase) (isVirtual : bool) (baseDefaultConstructor : ConstructorInfo) =
    let fields = [| for fi in targetFields -> struct (fi, tb.DefineFieldForProtobuf(fi)) |]
    let constructor =
        let paramType = if targetType.IsValueType then targetType.MakeByRefType() else targetType
        tb.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| paramType |])
    begin
        let gen = constructor.GetILGenerator()
        if not tb.IsValueType then // Call base class default constructor (if applicable)
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Call, match baseDefaultConstructor with | null -> typeof<obj>.GetConstructor [||] | ctr -> ctr)
        for (originField, surrogateField) in fields do
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Ldarg_1)
            gen.Emit(OpCodes.Call, originField.GetMethod)
            gen.Emit(OpCodes.Stfld, surrogateField)
        gen.Emit(OpCodes.Ret)
    end
    let extractMethod =
        let attr = if isVirtual then MethodAttributes.Public ||| MethodAttributes.Virtual else MethodAttributes.Public
        tb.DefineMethod("Extract", attr, targetType, [| |])
    begin
        let gen = extractMethod.GetILGenerator()
        for (_, surrogateField) in fields do
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Ldfld, surrogateField)
            emitStackTopZeroCheck gen surrogateField.FieldType
        match targetGenerate with
        | :? ConstructorInfo as ctr -> gen.Emit(OpCodes.Newobj, ctr)
        | :? MethodInfo as method -> gen.Emit(OpCodes.Call, method)
        | smth -> failwithf "Expected constructor or static method, but got %A" smth
        gen.Emit(OpCodes.Ret)
    end
    struct (constructor, extractMethod)

let private surrogatePrefix = "ProtoBuf.FSharp.Surrogates.Generated"

/// Emits a record surrogate. Intended to be used to support value type records ONLY since Protobuf-net at time of writing does not support custom ValueTypes/Structs.
let private emitRecordSurrogate (surrogateModule: ModuleBuilder) (recordType: Type) (defineSurrogateAsValueType: bool) =
    let genericArgs = getGenericArgs recordType
    let surrogateType =
        let name = sprintf "%s.%s" surrogatePrefix recordType.FullName
        let attr = TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
        if defineSurrogateAsValueType
        then surrogateModule.DefineType(name, attr, typeof<ValueType>)
        else surrogateModule.DefineType(name, attr)
    defineGenericArgs genericArgs surrogateType
    surrogateType.SetProtoContractAttribute(defineSurrogateAsValueType)
    let defaultConstructor =
        if surrogateType.IsValueType
        then ValueNone
        else surrogateType.DefineDefaultConstructor MethodAttributes.Public |> ValueSome

    let struct (constructor, extractMethod) =
        emitSurrogateContent surrogateType recordType
            (FSharpType.GetRecordFields(recordType, true))
            (FSharpValue.PreComputeRecordConstructorInfo(recordType, true))
            false null

    // Define op_Explicit methods that Protobuf calls to create recordType from surrogate.
    let conv = surrogateType.DefineOpExplicit(surrogateType, recordType)
    let gen = conv.GetILGenerator()
    gen.Emit((if surrogateType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
    gen.Emit(OpCodes.Call, extractMethod)
    gen.Emit(OpCodes.Ret)

    // Define op_Explicit methods that Protobuf calls to create surrogate from recordType.
    let conv = surrogateType.DefineOpExplicit(recordType, surrogateType)
    let gen = conv.GetILGenerator()
    let argIsNotNull = gen.DefineLabel()
    if not recordType.IsValueType then // Check if argument is reference type and is not null
        gen.Emit(OpCodes.Ldarg_0)
        gen.Emit(OpCodes.Brtrue, argIsNotNull)
        match defaultConstructor with
        | ValueSome ctr -> gen.Emit(OpCodes.Newobj, ctr)
        | ValueNone -> emitDefaultValueViaCell gen surrogateType
        gen.Emit(OpCodes.Ret)
    gen.MarkLabel(argIsNotNull)
    gen.Emit((if recordType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
    gen.Emit(OpCodes.Newobj, constructor)
    gen.Emit(OpCodes.Ret)

    surrogateType.CreateTypeInfo()

/// Puts tag of union in arg 0 (of type 'unionType') on top of evaluation stack
let private emitGetUnionTag (gen : ILGenerator) (unionType: Type) =
    gen.Emit((if unionType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
    match FSharpValue.PreComputeUnionTagMemberInfo(unionType, true) with
    | :? PropertyInfo as tag -> gen.Emit(OpCodes.Call, tag.GetMethod)
    | :? MethodInfo as tag-> gen.Emit(OpCodes.Call, tag)
    | smth -> failwithf "Unexpected tag member: %A" smth

/// Enumerate subtypes of union type that (maybe) represent union cases
let relevantUnionSubtypes (unionType: Type) = seq {
    for tt in unionType.GetNestedTypes(BindingFlags.Public ||| BindingFlags.NonPublic) do
        let subtype =
            if unionType.IsGenericType && tt.IsGenericTypeDefinition
                then tt.MakeGenericType(unionType.GetGenericArguments())
                else tt
        if subtype.IsSubclassOf unionType then
            yield subtype
}

/// Emit surrogate for UnionType in that style:
/// stuct UnionSurrogate {
///     public enum UnionTags { UnionTags_A, UnionTags_B, UnionTags_C, ... }
///     public class CaseA { ... }
///     public class CaseC { ... }
///     ...
///     public UnionTags Tag;
///     public CaseA DataA;
///     public CaseC DataC;
///     ...
/// }
let private emitUnionSurrogateWithTag (surrogateModule: ModuleBuilder) (unionType: Type) =
    let genericArgs = getGenericArgs unionType
    let surrogateType =
        let name = sprintf "%s.%s" surrogatePrefix unionType.FullName
        let attr = TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
        surrogateModule.DefineType(name, attr, typeof<ValueType>)
    defineGenericArgs genericArgs surrogateType
    surrogateType.SetProtoContractAttribute(false)

    let tagEnum =
        let name = sprintf "UnionTags%i" (Interlocked.Increment &uniqueNameCounter)
        surrogateType.DefineNestedType(name, TypeAttributes.NestedPublic ||| TypeAttributes.Sealed, typeof<Enum>, null)
    tagEnum.DefineField("value__", typeof<int>, FieldAttributes.Private ||| FieldAttributes.SpecialName) |> ignore

    let surrogateTagField = surrogateType.DefineField("Tag", tagEnum, FieldAttributes.Public)
    surrogateTagField.SetProtoMemberAttribute(1)

    let cases = [|
        for caseInfo in FSharpType.GetUnionCases(unionType, true) ->
            let enumCase =
                let name = sprintf "%s_%s" tagEnum.Name caseInfo.Name
                tagEnum.DefineField(name, tagEnum, FieldAttributes.Public ||| FieldAttributes.Literal ||| FieldAttributes.Static)
            enumCase.SetConstant(caseInfo.Tag)
            let caseData =
                match caseInfo.GetFields() with
                | [||] -> ValueNone
                | caseFields ->
                    let subtype =
                        let attr = TypeAttributes.NestedPublic ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
                        surrogateType.DefineNestedType("Case" + caseInfo.Name, attr)
                    defineGenericArgs genericArgs subtype
                    subtype.SetProtoContractAttribute(false)
                    subtype.DefineDefaultConstructor MethodAttributes.Public |> ignore
                    let struct (constructor, extractMethod) =
                        emitSurrogateContent subtype unionType caseFields (FSharpValue.PreComputeUnionConstructorInfo(caseInfo, true)) false null
                    subtype.CreateTypeInfo() |> ignore
                    let caseDataField = surrogateType.DefineField("Data" + caseInfo.Name, subtype, FieldAttributes.Public)
                    caseDataField.SetProtoMemberAttribute(2 + caseInfo.Tag)
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
            | ValueNone -> gen.Emit(OpCodes.Call, FSharpValue.PreComputeUnionConstructorInfo(caseInfo, true))
            | ValueSome struct (caseDataField, _, extractMethod) ->
                gen.Emit(OpCodes.Ldarga_S, 0)
                gen.Emit(OpCodes.Ldfld, caseDataField)
                gen.Emit(OpCodes.Call, extractMethod)
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
        if not unionType.IsValueType then // Check if argument is reference type and is null
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Brfalse, endLabel)

        let jumpTable = Array.init (Array.length cases) (ignore >> gen.DefineLabel)
        emitGetUnionTag gen unionType
        gen.Emit(OpCodes.Switch, jumpTable) // Dispatch on int union tag
        gen.Emit(OpCodes.Br, endLabel)

        for (caseInfo, caseStructure) in cases do
            gen.MarkLabel(jumpTable.[ caseInfo.Tag ])
            caseStructure |> ValueOption.iter (fun struct (caseDataField, caseConstructor, _) ->
                // Create additional data for this union case and store it into corresponding field
                gen.Emit(OpCodes.Ldloca_S, resultCell)
                gen.Emit((if unionType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
                gen.Emit(OpCodes.Newobj, caseConstructor)
                gen.Emit(OpCodes.Stfld, caseDataField)
            )
            // Write tag
            gen.Emit(OpCodes.Ldloca_S, resultCell)
            gen.Emit(OpCodes.Ldc_I4, caseInfo.Tag)
            gen.Emit(OpCodes.Stfld, surrogateTagField)
            gen.Emit(OpCodes.Br, endLabel)

        gen.MarkLabel(endLabel)
        gen.Emit(OpCodes.Ldloc, resultCell)
        gen.Emit(OpCodes.Ret)
        conv

    // Create additional conversion operators for subtypes
    for subtype in relevantUnionSubtypes unionType do
        surrogateType
            .DefineOpExplicit(surrogateType, subtype)
            .GetILGenerator()
            .Emit(OpCodes.Jmp, fromSurrogate)

        surrogateType
            .DefineOpExplicit(subtype, surrogateType)
            .GetILGenerator()
            .Emit(OpCodes.Jmp, toSurrogate)

    tagEnum.CreateTypeInfo() |> ignore
    surrogateType.CreateTypeInfo ()

/// Emit surrogate for UnionType in that style:
/// stuct UnionSurrogate {
///     public abstract class Base { public abstract UnionType Extract();  }
///     public class CaseA : Base { ... }
///     public class CaseB : Base { ... }
///     ...
///     Base Tag;
/// }
let private emitUnionSurrogateWithSubtypes (surrogateModule: ModuleBuilder) (unionType: Type) =
    let genericArgs = getGenericArgs unionType
    let surrogateType =
        let name = sprintf "%s.%s" surrogatePrefix unionType.FullName
        let attr = TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
        surrogateModule.DefineType(name, attr, typeof<ValueType>)
    defineGenericArgs genericArgs surrogateType
    surrogateType.SetProtoContractAttribute(false)

    let caseBaseType =
        let attr = TypeAttributes.NestedPublic ||| TypeAttributes.Abstract ||| TypeAttributes.Serializable
        surrogateType.DefineNestedType("Base", attr)
    defineGenericArgs genericArgs caseBaseType
    caseBaseType.SetProtoContractAttribute(false)
    let baseDefaultConstructor = caseBaseType.DefineDefaultConstructor MethodAttributes.Public
    let extractBaseMethod =
        let attr = MethodAttributes.Public ||| MethodAttributes.Virtual ||| MethodAttributes.Abstract
        caseBaseType.DefineMethod("Extract", attr, unionType, [| |])
    caseBaseType.CreateTypeInfo() |> ignore

    let surrogateTagField = surrogateType.DefineField("Tag", caseBaseType, FieldAttributes.Public)
    surrogateTagField.SetProtoMemberAttribute(1)

    let cases = [|
        for caseInfo in FSharpType.GetUnionCases(unionType, true) ->
            let subtype =
                let attr = TypeAttributes.NestedPublic ||| TypeAttributes.Sealed ||| TypeAttributes.Serializable
                surrogateType.DefineNestedType("Case" + caseInfo.Name, attr, caseBaseType)
            defineGenericArgs genericArgs subtype
            subtype.SetProtoContractAttribute(false)
            let struct (constructor, extractMethod) =
                emitSurrogateContent subtype unionType
                    (caseInfo.GetFields())
                    (FSharpValue.PreComputeUnionConstructorInfo(caseInfo, true))
                    true baseDefaultConstructor
            subtype.DefineMethodOverride(extractMethod, if genericArgs.IsSome then TypeBuilder.GetMethod(caseBaseType, extractBaseMethod) else extractBaseMethod :> _) // Generic magic
            begin // DefineDefaultConstructor doesn't work here
                let ctr = subtype.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [| |])
                let gen = ctr.GetILGenerator()
                gen.Emit(OpCodes.Ldarg_0)
                gen.Emit(OpCodes.Call, baseDefaultConstructor)
                gen.Emit(OpCodes.Ret)
            end
            struct (caseInfo, subtype, constructor)
    |]

    // Define op_Explicit methods that Protobuf calls to create unionType from surrogate.
    let fromSurrogate =
        let conv = surrogateType.DefineOpExplicit(surrogateType, unionType)
        let gen = conv.GetILGenerator()
        gen.Emit((if surrogateType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
        gen.Emit(OpCodes.Ldfld, surrogateTagField)
        gen.Emit(OpCodes.Callvirt, extractBaseMethod)
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
        if not unionType.IsValueType then // Check if argument is reference type and is null
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Brfalse, endLabel)

        let jumpTable = Array.init (Array.length cases) (ignore >> gen.DefineLabel)
        emitGetUnionTag gen unionType
        gen.Emit(OpCodes.Switch, jumpTable) // Dispatch on int union tag
        gen.Emit(OpCodes.Br, endLabel)

        for (caseInfo, _, caseConstructor) in cases do
            gen.MarkLabel(jumpTable.[ caseInfo.Tag ])
            gen.Emit(OpCodes.Ldloca_S, resultCell)
            gen.Emit((if unionType.IsValueType then OpCodes.Ldarga_S else OpCodes.Ldarg), 0)
            gen.Emit(OpCodes.Newobj, caseConstructor)
            gen.Emit(OpCodes.Stfld, surrogateTagField)
            gen.Emit(OpCodes.Br, endLabel)

        gen.MarkLabel(endLabel)
        gen.Emit(OpCodes.Ldloc, resultCell)
        gen.Emit(OpCodes.Ret)
        conv

    // Create additional conversion operators for subtypes
    for subtype in relevantUnionSubtypes unionType do
        surrogateType
            .DefineOpExplicit(surrogateType, subtype)
            .GetILGenerator()
            .Emit(OpCodes.Jmp, fromSurrogate)

        surrogateType
            .DefineOpExplicit(subtype, surrogateType)
            .GetILGenerator()
            .Emit(OpCodes.Jmp, toSurrogate)

    // This method is called by Serialiser.registerSurrogate with reflection
    // It calls AddSubType for all subtypes of Base (attribute doesn't appear to work with emitted classes for some reason)
    begin
        let method = surrogateType.DefineMethod("RegisterIntoModel", MethodAttributes.Public ||| MethodAttributes.Static, null, [| typeof<ProtoBuf.Meta.RuntimeTypeModel> |])
        let gen = method.GetILGenerator()
        let metaTypeCell = gen.DeclareLocal(typeof<ProtoBuf.Meta.MetaType>)
        gen.Emit(OpCodes.Ldarg_0)
        gen.Emit(OpCodes.Ldtoken, substituteGenericArgs genericArgs caseBaseType)
        gen.Emit(OpCodes.Ldc_I4_1)
        gen.Emit(OpCodes.Call, typeof<ProtoBuf.Meta.RuntimeTypeModel>.GetMethod("Add"))
        gen.Emit(OpCodes.Stloc, metaTypeCell)
        for (caseInfo, subclass, _) in cases do
            gen.Emit(OpCodes.Ldloc, metaTypeCell)
            gen.Emit(OpCodes.Ldc_I4, 1000 + caseInfo.Tag)
            gen.Emit(OpCodes.Ldtoken, substituteGenericArgs genericArgs subclass)
            gen.Emit(OpCodes.Call, typeof<ProtoBuf.Meta.MetaType>.GetMethod("AddSubType", [| typeof<int> ; typeof<Type> |]))
            gen.Emit(OpCodes.Pop)
        gen.Emit(OpCodes.Ret)
    end

    let surrogate = surrogateType.CreateTypeInfo ()
    for (_, sub, _) in cases do
        sub.CreateTypeInfo() |> ignore
    surrogate


let private surrogateAssembly = AssemblyBuilder.DefineDynamicAssembly(AssemblyName("SurrogateAssembly"), AssemblyBuilderAccess.Run)
let private surrogateModule = surrogateAssembly.DefineDynamicModule "SurrogateModule"
let private surrogateCache =
    ConcurrentDictionary<Type, Lazy<Type>> (seq {
        KeyValuePair(typedefof<_ option>, lazy typedefof<Surrogates.Optional<_>>)
        KeyValuePair(typedefof<_ list>, lazy typedefof<Surrogates.ListSurrogate<_>>)
        KeyValuePair(typedefof<Set<_>>, lazy typedefof<Surrogates.SetSurrogate<_>>)
        KeyValuePair(typedefof<Map<_, _>>, lazy typedefof<Surrogates.MapSurrogate<_, _>>)
    })

let private makeSurrogate (typeToAdd : Type) =
    match typeToAdd with
    | t when FSharpType.IsUnion(t, true) ->
        if t.IsValueType then
            lazy (emitUnionSurrogateWithTag surrogateModule typeToAdd :> Type)
        else
            lazy (emitUnionSurrogateWithSubtypes surrogateModule typeToAdd :> Type)
    | t when FSharpType.IsRecord(t, true) ->
        lazy (emitRecordSurrogate surrogateModule typeToAdd true :> Type)
    | t ->
        failwithf "No surrogate construction method for type %A" t

let getSurrogate (typeToAdd : Type) =
    if typeToAdd.IsGenericType then
        let surrogateDef = surrogateCache.GetOrAdd(typeToAdd.GetGenericTypeDefinition(), makeSurrogate).Value
        surrogateDef.MakeGenericType(typeToAdd.GetGenericArguments())
    else
        surrogateCache.GetOrAdd(typeToAdd, makeSurrogate).Value


let private factoryCache = ConcurrentDictionary<Type, Lazy<MethodInfo>>()

let getFactory (typeToAdd : Type) zeroValuesForFields =
    factoryCache.GetOrAdd(typeToAdd, fun _ -> lazy emitFactory typeToAdd zeroValuesForFields).Value
