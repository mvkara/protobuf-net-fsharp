namespace ProtoBuf.FSharpHelpers.Unit

open Expecto
open FsCheck
open System.IO
open ProtoBuf.FSharpHelpers
open ProtoBuf
open ProtoBuf.Meta
open Expecto.Expect
open System
open System.Collections.Generic
open System.Linq
open System.Runtime.CompilerServices

[<TestName("Standard record with a simple C# list")>]
type TestRecordOne = {
    One: string
    Two: int
    Three: string list
}

module TestRecordRoundtrip = 

    type DataGenerator =
            
        static member GenerateNonNullString() : Arbitrary<string> = 
            [ 1 .. 20 ]
            |> Gen.elements
            |> Gen.map (string)
            |> Arb.fromGen
                
    let propertyToTest<'t when 't : equality> cleanupFunc (typeToTest: 't)  = 
        let model = 
            RuntimeTypeModel.Create() 
            //|> CollectionRegistration.registerListTypeIntoModel<string>
            |> ProtobufNetFSharp.registerRecordIntoModel true typeof<'t>

        model.CompileInPlace()
        use ms = new MemoryStream()
        ProtobufNetFSharp.serialise model ms typeToTest
        ms.Seek(0L, SeekOrigin.Begin) |> ignore
        let rtData = ProtobufNetFSharp.deserialise<'t> model ms
        equal (cleanupFunc rtData) (cleanupFunc typeToTest) "Type not equal"
    
    let config = { Config.QuickThrowOnFailure with Arbitrary = [ typeof<DataGenerator> ] }

    let emptySingletonListForEqualityComparison = new List<string>() :> IList<string>

    let cleanupNullForLists (t: TestRecordOne) = t//{ t with Three = match t.Three with | null -> [||] | _ -> t.Three }

    let buildTest<'t when 't : equality> cleanupNullForLists = 
        let testNameAttribute = typeof<'t>.GetCustomAttributes(typeof<TestNameAttribute>, true) |> Seq.head :?> TestNameAttribute
        testCase testNameAttribute.Name <| fun () -> Check.One(config, (propertyToTest<'t> cleanupNullForLists))

    [<Tests>]
    let test() = 
        testList 
            "Record Test Cases" 
            [ buildTest<TestRecordOne> cleanupNullForLists ]