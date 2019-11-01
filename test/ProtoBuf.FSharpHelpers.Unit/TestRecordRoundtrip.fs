namespace ProtoBuf.FSharpHelpers.Unit

open Expecto
open FsCheck
open System.IO
open ProtoBuf.FSharpHelpers
open ProtoBuf.Meta
open Expecto.Expect
open System.Collections.Generic

[<TestName("Standard record with a simple C# list")>]
type TestRecordOne = 
    {
        One: string
        Two: int
        Three: string[]
        Four: string option
    }

[<TestName("All optional fields")>]
type TestRecordTwo = { TwoOne: string option; TwoTwo: int option }

module TestRecordRoundtrip = 

    // F# does not allow nulls although FsCheck tries to stress C# interoperability.
    // Disabling it here because this library is for wrapping F# types only.
    type DataGenerator =
        static member Generate() : Arbitrary<string[]> = 
            Gen.oneof ([ "One"; "Two"; "" ] |> List.map Gen.constant) 
            |> Gen.listOf
            |> Gen.map List.toArray
            |> Arb.fromGen
            
        static member GenerateNonNullString() : Arbitrary<string> = Arb.Default.StringWithoutNullChars().Generator |> Gen.map (fun x -> x.Get) |> Gen.filter (box >> Operators.isNull >> not) |> Arb.fromGen

    let propertyToTest<'t when 't : equality> (typeToTest: 't)  = 
        let model = 
            RuntimeTypeModel.Create()
            |> ProtobufNetSerialiser.registerRecordIntoModel<'t>
        let m = model.Add(typeof<TestRecordOne>, true)
        m.UseConstructor <- false
        model.CompileInPlace()
        let cloned = model.DeepClone(typeToTest)
        equal (unbox cloned) typeToTest "Protobuf deep clone"
        use ms = new MemoryStream()
        ProtobufNetSerialiser.serialise model ms typeToTest
        ms.Seek(0L, SeekOrigin.Begin) |> ignore
        let rtData = ProtobufNetSerialiser.deserialise<'t> model ms
        equal rtData typeToTest "Type not equal"
    
    let config = { Config.QuickThrowOnFailure with Arbitrary = [ typeof<DataGenerator> ]; }

    let emptySingletonListForEqualityComparison = new List<string>() :> IList<string>

    let buildTest<'t when 't : equality> = 
        let testNameAttribute = typeof<'t>.GetCustomAttributes(typeof<TestNameAttribute>, true) |> Seq.head :?> TestNameAttribute
        testCase testNameAttribute.Name <| fun () -> Check.One(config, propertyToTest<'t>)

    [<Tests>]
    let test() = 
        testList 
            "Record Test Cases" 
            [ buildTest<TestRecordOne>; buildTest<TestRecordTwo>  ]