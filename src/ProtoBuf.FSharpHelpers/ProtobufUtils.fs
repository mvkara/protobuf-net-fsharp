namespace ProtoBuf.FSharpHelpers

open ProtoBuf
open ProtoBuf.Meta
open FSharp.Reflection
open System
open System.Linq.Expressions
open System.Reflection
open System.IO
open System.Reflection
open System.Collections.Generic
open System.Collections.Concurrent
open Microsoft.FSharp.Quotations.Patterns
open System.Reflection.Emit

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

    let constructObjectWithoutConstructor (typeToConstruct: Type) = 
        System.Runtime.Serialization.FormatterServices.GetUninitializedObject(typeToConstruct)

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
        let item = MethodHelpers.constructObjectWithoutConstructor typeof<'t> :?> 't
        match MethodHelpers.staticSetters.TryGetValue(typeof<'t>) with
        | (true, action) -> (action :?> Action<'t>).Invoke(item)
        | (false, _) -> ()
        item

module ProtobufNetSerialiser =
    
    let private zipWithOneBasedIndex item = item |> Seq.zip (Seq.initInfinite id |> Seq.skip 1)

    let private processFieldsAndCreateFieldSetters (typeToAdd: Type) (metaType: MetaType )= 
        metaType.UseConstructor <- false
        let _, fieldSetterDelegates =
        
            typeToAdd.GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.GetField)
            |> Array.fold 
                (fun (index, delegates) fieldInfo -> 
                    //let mt = mt.Add(fieldInfo.Name)
                    let fieldModel = metaType.AddField(index, fieldInfo.Name.Replace("@", ""))
                    fieldModel.BackingMember <- fieldInfo
                    fieldModel.OverwriteList <- true
                    
                    match GenericSetterFactory.GetSetterCallbackIfApplicableForFieldInfo fieldInfo with
                    | Some(d) -> (index+1, d :: delegates)
                    | None -> (index+1, delegates)
                    )
                (1, [])

        if not fieldSetterDelegates.IsEmpty
        then
            let setterDelegate = Delegate.Combine(fieldSetterDelegates |> List.toArray)
            GenericSetterFactory.SetNewCreationSetterDelegate typeToAdd setterDelegate
            let factoryMethodInfo = MethodHelpers.getMethodInfo <@ GenericSetterFactory.CreateOrDefault @> [| typeToAdd |]
            metaType.SetFactory(factoryMethodInfo)
        else metaType            

    let registerUnionIntoModel<'t> (model: RuntimeTypeModel) =

        let unionType = typeof<'t>
        let unionCaseData = FSharpType.GetUnionCases(unionType, true)
        
        // Register the supertype in all cases
        let mt = model.Add(unionType, true)
        mt.UseConstructor <- false
        for (fId, f) in unionType.GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.GetField) |> zipWithOneBasedIndex do
            mt.AddField(fId, f.Name) |> ignore

        // If there are no fields in any properties then we can assume the F# compiler has compiled
        // the class in a non-flat fashion. Structs are still compiled in a flat way (F# 4.1+ struct DU's).
        let isReferenceMulticaseDuWithPayload = not (typeof<'t>.IsValueType || unionCaseData |> Seq.collect (fun x -> x.GetFields()) |> Seq.isEmpty)
        
        if isReferenceMulticaseDuWithPayload
        then 
            for ucd in unionCaseData do
                // Cases with no fields are deemed private by the model (if no fields then it is a private class with a _ in its name)
                let suffix = if ucd.GetFields() |> Seq.isEmpty then "_" + ucd.Name else ucd.Name
                let typeToAdd = unionType.Assembly.GetType(ucd.DeclaringType.FullName + "+" + suffix)
                
                let caseTypeModel = model.Add(typeToAdd, true)
                
                mt.AddSubType(1000 + ucd.Tag, typeToAdd) |> ignore
                caseTypeModel.UseConstructor <- false
                
                processFieldsAndCreateFieldSetters typeToAdd caseTypeModel |> ignore
        model  

    let registerRecordIntoModel<'t> (model: RuntimeTypeModel) = 
        let metaType = model.Add(typeof<'t>, false)
        metaType.UseConstructor <- false
        let metaType = processFieldsAndCreateFieldSetters typeof<'t> metaType 
        model          

    let registerTypeIntoModel<'t> (model: RuntimeTypeModel) = 
        if FSharpType.IsRecord typeof<'t>
        then registerRecordIntoModel<'t> model
        elif FSharpType.IsUnion typeof<'t>
        then registerUnionIntoModel<'t> model
        else 
            model.Add(typeof<'t>, true) |> ignore
            model

    let serialise (model: RuntimeTypeModel) (stream: Stream) (o: 't) = model.Serialize(stream, o)

    let deserialise<'t> (model: RuntimeTypeModel) (stream: Stream) = model.Deserialize(stream, null, typeof<'t>) :?> 't

    let defaultModel = RuntimeTypeModel.Default