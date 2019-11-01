namespace ProtoBuf.FSharpHelpers.Unit

open Expecto
open FsCheck
open System.IO
open ProtoBuf.FSharpHelpers
open ProtoBuf
open ProtoBuf.Meta
open Expecto.Expect
open System

// F# does not allow nulls although FsCheck tries to stress C# interoperability.
// Disabling it here because this library is for wrapping F# types only.
type DataGenerator =
    static member Generate() : Arbitrary<string[]> = 
        Gen.oneof ([ "One"; "Two"; "" ] |> List.map Gen.constant) 
        |> Gen.listOf
        |> Gen.map List.toArray
        |> Arb.fromGen
        
    static member GenerateNonNullString() : Arbitrary<string> = Arb.Default.StringWithoutNullChars().Generator |> Gen.map (fun x -> x.Get) |> Gen.filter (box >> Operators.isNull >> not) |> Arb.fromGen

module ExampleTypesInsideModule = 

    [<RequireQualifiedAccess; TestName("Single case DU")>]
    type UnionOne = | One

    [<RequireQualifiedAccess; TestName("Multi case DU, No Fields")>]
    type UnionTwo = | One | Two

    [<RequireQualifiedAccess; TestName("One has string")>]
    type UnionThree = | One | Two of string

    [<RequireQualifiedAccess; TestName("All have fields")>]
    type UnionFour = | One of int | Two of string

    [<RequireQualifiedAccess; TestName("More than one field per case")>]
    type UnionFive = | One of int | Two of test1: string * test2: int

    [<RequireQualifiedAccess; TestName("More than one field per case; has array type")>]
    type UnionSix = | One of int | Two of test1: string * test2: int array

    [<RequireQualifiedAccess; TestName("More than one field per case; has array type and option type")>]
    type UnionSeven = | One of int option | Two of test1: int option * test2: int array

module TestUnionRoundtrip =

    let propertyToTest<'t when 't : equality> (typeToTest: 't) = 
        let model = RuntimeTypeModel.Create() |> ProtobufNetSerialiser.registerUnionIntoModel<'t>
        model.CompileInPlace()
        let cloned = model.DeepClone(typeToTest)
        equal (unbox cloned) (typeToTest) "Protobuf deep clone"
        use ms = new MemoryStream()
        ProtobufNetSerialiser.serialise model ms typeToTest
        ms.Seek(0L, SeekOrigin.Begin) |> ignore
        let rtData = ProtobufNetSerialiser.deserialise<'t> model ms
        equal rtData typeToTest "Type not equal"
    
    let buildTest<'t when 't : equality>() = 
        let config = { Config.QuickThrowOnFailure with Arbitrary = [ typeof<DataGenerator> ] }
        let testNameAttribute = typeof<'t>.GetCustomAttributes(typeof<TestNameAttribute>, true) |> Seq.head :?> TestNameAttribute
        testCase testNameAttribute.Name <| fun () -> Check.One(config, propertyToTest<'t>)

    [<Tests>]
    let test() = 
        testList 
            "Union Test Cases" 
            [ buildTest<ExampleTypesInsideModule.UnionOne>()
              buildTest<ExampleTypesInsideModule.UnionTwo>()
              buildTest<ExampleTypesInsideModule.UnionThree>()
              buildTest<ExampleTypesInsideModule.UnionFour>()
              buildTest<ExampleTypesInsideModule.UnionFive>()
              buildTest<ExampleTypesInsideModule.UnionSix>()
              buildTest<ExampleTypesInsideModule.UnionSeven>() ]