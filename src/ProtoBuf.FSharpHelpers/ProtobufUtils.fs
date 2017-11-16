namespace ProtoBuf.FSharpHelpers

open ProtoBuf
open ProtoBuf.Meta
open FSharp.Reflection

module ProtobufNetSerialiser =
    open System.IO
    open System.Reflection

    let zipWithOneBasedIndex item = item |> Seq.zip (Seq.initInfinite id |> Seq.skip 1)

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
                
                typeToAdd.GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.GetField)
                |> Array.map (fun x -> x.Name)
                |> caseTypeModel.Add
                |> (fun x -> x.UseConstructor <- false)
        model

    let registerRecordIntoModel<'t> (model: RuntimeTypeModel) = 
        let mt = model.Add(typeof<'t>, false)
        mt.UseConstructor <- false
        typeof<'t>.GetFields(BindingFlags.NonPublic ||| BindingFlags.Instance ||| BindingFlags.GetField)
        |> Array.map (fun x -> x.Name)
        |> mt.Add
        |> (fun x -> x.UseConstructor <- false)

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