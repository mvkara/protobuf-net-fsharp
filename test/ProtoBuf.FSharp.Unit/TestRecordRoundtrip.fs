namespace ProtoBuf.FSharp.Unit

open Expecto
open FsCheck
open System.IO
open ProtoBuf.FSharp
open ProtoBuf.Meta
open Expecto.Expect
open System.Collections.Generic
open System

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

[<TestName("Record CLIMutable type"); CLIMutable>]
type TestRecordThree = { Three: string; Four: int }

[<TestName("Record with mutable field")>]
type TestRecordFour = {
    mutable Flag : bool
    String : string
}


[<Struct; TestName("Struct Record")>]
type TestRecordFive = {
    Flag : bool
    String : string
}

type TestEnum =
    | OptionA = 1uy
    | OptionB = 2uy
    | OptionC = 69uy

[<TestName("Internal Record")>]
type TestRecordSix = internal {
    Field : struct (int * bool * int64)
    Number : int64
    DecimalNumber : decimal
    EnumField : TestEnum
    String : string
    Date : System.DateTime
}

[<Struct; TestName("Struct Record with weird field names")>]
type TestRecordSeven = {
    ``__$uperF!eld__`` : bool
    ``String#`` : string
}

type InnerNestedRecordWithCollections = {
    ArrayData: int array
    StringData: string
}

[<TestName("Struct nested record with arrays and strings", [| typeof<InnerNestedRecordWithCollections> |])>]
type NestedRecordWithZeroValues = {
    NestedCollectedData: InnerNestedRecordWithCollections array
    NestedData: InnerNestedRecordWithCollections
    Data: int array
    Name: string
}

[<Struct; TestName("Struct record with primitive collections")>]
type StructRecordWithCollectionTestCases = {
    TextCollection: string array
    Data: int array
    Name: string
}

[<Struct; TestName("Struct record with inner complex types")>]
type StructRecordWithNestedTypes = {
    DataCollection: InnerNestedRecordWithCollections array
    Data: InnerNestedRecordWithCollections
}

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

    let roundtripSerialise (typeToTest: 't) (otherDependentRecordTypes: Type array) = 
        let model = 
            RuntimeTypeModel.Create()
            |> Serialiser.registerRecordIntoModel<'t>

        for dependentRecordType in otherDependentRecordTypes do
            Serialiser.registerRuntimeTypeIntoModel dependentRecordType model |> ignore

        let cloned = model.DeepClone(typeToTest)
        equal (unbox cloned) typeToTest "Protobuf deep clone"
        use ms = new MemoryStream()
        Serialiser.serialise model ms typeToTest
        ms.Seek(0L, SeekOrigin.Begin) |> ignore
        Serialiser.deserialise<'t> model ms

    let testRoundtrip<'t when 't : equality> otherDependentRecordTypes (typeToTest: 't)  = 
        let rtData = roundtripSerialise typeToTest otherDependentRecordTypes
        equal rtData typeToTest "Type not equal"
    
    let config = { Config.QuickThrowOnFailure with Arbitrary = [ typeof<DataGenerator> ]; }

    let emptySingletonListForEqualityComparison = new List<string>() :> IList<string>

    let buildTest<'t when 't : equality> = 
        let testNameAttribute = typeof<'t>.GetCustomAttributes(typeof<TestNameAttribute>, true) |> Seq.head :?> TestNameAttribute
        testCase testNameAttribute.Name <| fun () -> Check.One(config, testRoundtrip<'t> testNameAttribute.DependentTypeParamters)

    let manualTestCases = [
        testCase "Can serialise empty array, string and option" <| fun () -> testRoundtrip [||] { One = ""; Two = 1; Three = [||]; Four = None } 
        testCase "Can serialise option containing value" <| fun () -> testRoundtrip [||] { One = ""; Two = 1; Three = [||]; Four = Some "TEST" }
        testCase "Can serialise string, array and option containing value" <| fun () -> testRoundtrip [||] { One = "TEST"; Two = 1; Three = [| "TEST1" |]; Four = Some "TEST" }
    ]

    [<Tests>]
    let test() = 
        testList 
            "Record Test Cases" 
            [ yield buildTest<TestRecordOne>; 
              yield buildTest<TestRecordTwo>
              yield buildTest<TestRecordThree>
              yield buildTest<TestRecordFour>
              yield buildTest<TestRecordFive>
              yield buildTest<TestRecordSix>
              yield buildTest<TestRecordSeven>
              yield buildTest<NestedRecordWithZeroValues>
              yield buildTest<StructRecordWithCollectionTestCases> // TODO: Fix struct record logic
              yield buildTest<StructRecordWithNestedTypes> // TODO: Fix struct record logic
              yield! manualTestCases
            ]