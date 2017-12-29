namespace ProtoBuf.FSharpHelpers

open ProtoBuf
open ProtoBuf.Meta
open FSharp.Reflection
open CollectionRegistration

module ProtobufNetFSharp =
    open System.IO
    open System.Reflection

    let private zipWithOneBasedIndex item = item |> Seq.zip (Seq.initInfinite id |> Seq.skip 1)

    let inline attemptToRegisterListType includeCollectionTypes memberType model = 
        if includeCollectionTypes 
        then registerCollectionWithReflectedTypeIntoModel model memberType
        else model

    let registerUnionIntoModel includeCollectionTypes unionType (model: RuntimeTypeModel) =
        let unionCaseData = FSharpType.GetUnionCases(unionType, true)
        
        // Register the supertype in all cases
        let mt = model.Add(unionType, true)
        mt.UseConstructor <- false
        
        let model = 
            unionType.GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.GetField) 
            |> zipWithOneBasedIndex
            |> Seq.fold
                (fun m (fId, (f: FieldInfo)) -> 
                    let newModel = attemptToRegisterListType includeCollectionTypes f.FieldType m
                    mt.AddField(fId, f.Name) |> ignore
                    newModel)
                model                

        // If there are no fields in any properties then we can assume the F# compiler has compiled
        // the class in a non-flat fashion. Structs are still compiled in a flat way (F# 4.1+ struct DU's).
        let isReferenceMulticaseDuWithPayload = 
            not (unionType.IsValueType || unionCaseData |> Seq.collect (fun x -> x.GetFields()) |> Seq.isEmpty)
        
        if isReferenceMulticaseDuWithPayload
        then 
            unionCaseData
            |> Array.fold 
                (fun (newModel: RuntimeTypeModel) (ucd: UnionCaseInfo) ->
                    // Cases with no fields are deemed private by the model (if no fields then it is a private class with a _ in its name)
                    let suffix = if ucd.GetFields() |> Seq.isEmpty then "_" + ucd.Name else ucd.Name
                    let typeToAdd = unionType.Assembly.GetType(ucd.DeclaringType.FullName + "+" + suffix)
                    
                    let caseTypeModel = newModel.Add(typeToAdd, true)
                    
                    mt.AddSubType(1000 + ucd.Tag, typeToAdd) |> ignore
                    caseTypeModel.UseConstructor <- false
                    
                    typeToAdd.GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.GetField)
                    |> Array.map (fun x -> x.Name, x.FieldType)
                    |> Array.fold 
                        (fun newModel (name, ft) -> 
                            let newModel = attemptToRegisterListType includeCollectionTypes ft newModel
                            caseTypeModel.Add(name) |> ignore
                            caseTypeModel.UseConstructor <- false
                            newModel)
                        newModel
                    )
                model                
        else model

    /// Registers a record into the runtime type model.
    /// <param name="includeCollectionTypes">If the type contains any collection types they will be registered into the model</param>
    let registerRecordIntoModel includeCollectionTypes recordType (model: RuntimeTypeModel) = 
        let mt = model.Add(recordType, false)
        mt.UseConstructor <- false
        recordType.GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.GetField)
        |> Array.map (fun x -> (x.Name, x.FieldType))
        |> Array.fold 
            (fun model (name, memberType) -> 
                let newModel = attemptToRegisterListType includeCollectionTypes memberType model
                let mt = mt.Add(name)
                mt.UseConstructor <- false
                newModel)
            model            

    let registerTypeIntoModel includeCollectionTypes typeToRegister (model: RuntimeTypeModel) = 
        if FSharpType.IsRecord typeToRegister
        then registerRecordIntoModel includeCollectionTypes typeToRegister model
        elif FSharpType.IsUnion typeToRegister
        then registerUnionIntoModel includeCollectionTypes typeToRegister model
        else 
            model.Add(typeToRegister, true) |> ignore
            model

    let serialise (model: RuntimeTypeModel) (stream: Stream) (o: 't) = model.Serialize(stream, o)

    let deserialise<'t> (model: RuntimeTypeModel) (stream: Stream) = model.Deserialize(stream, null, typeof<'t>) :?> 't

    let defaultModel = RuntimeTypeModel.Default