namespace ProtoBuf.FSharp.Unit

open Expecto
open Expecto.Expect
open ProtoBuf.FSharp
open ProtoBuf.Meta

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

    [<RequireQualifiedAccess; TestName("Single case union with data")>]
    type UnionEight = | One of int option * two: int array

    [<TestName("Union with generic; two cases")>]
    type SerialisableOption<'t> =
        | SerialisableSome of 't
        | SerialisableNone

    [<TestName("Union with generic; single case union")>]
    type Wrapper<'t> = | Wrapper of 't

    [<TestName("More than 4 cases; one case with no fields")>]
    type UnionNine =
    | CaseOne of numbers: int array // If any of the above show it.
    | CaseTwo of strings: string array
    | CaseThreee of singleData: string
    | CaseFour

    [<Struct; TestName("Value union with no data inside")>]
    type ValueUnionNoData =
    | CaseOne
    | CaseTwo
    | CaseThreee

module TestUnionRoundtrip =
    // This test is just to show how the schema will be look like for other consumers. It is expected to fail so isn't used normally.
    let manualTest =
        testCase
            "Generate schema"
            (fun () ->
                let model = RuntimeTypeModel.Create("") |> Serialiser.registerUnionIntoModel<ExampleTypesInsideModule.UnionNine>
                model.CompileInPlace()
                let schema = model.GetSchema(typeof<ExampleTypesInsideModule.UnionNine>)
                equal schema "" "Schema generated")

    [<Tests>]
    let test() =
        testList "Union Test Cases" [ //manualTest
            Roundtrip.buildProperty<ExampleTypesInsideModule.UnionOne>()
            Roundtrip.buildProperty<ExampleTypesInsideModule.UnionTwo>()
            Roundtrip.buildProperty<ExampleTypesInsideModule.UnionThree>()
            Roundtrip.buildProperty<ExampleTypesInsideModule.UnionFour>()
            Roundtrip.buildProperty<ExampleTypesInsideModule.UnionFive>()
            Roundtrip.buildProperty<ExampleTypesInsideModule.UnionSix>()
            Roundtrip.buildProperty<ExampleTypesInsideModule.UnionSeven>()
            Roundtrip.buildProperty<ExampleTypesInsideModule.UnionEight>()
            Roundtrip.buildProperty<ExampleTypesInsideModule.SerialisableOption<string>>()
            Roundtrip.buildProperty<ExampleTypesInsideModule.Wrapper<string>>()
            Roundtrip.buildProperty<ExampleTypesInsideModule.UnionNine>()
            Roundtrip.buildProperty<ValueOption<string>>()
            Roundtrip.buildProperty<Result<int, bool>>()
            Roundtrip.buildProperty<Result<string, int[]>>()
            Roundtrip.buildProperty<ExampleTypesInsideModule.ValueUnionNoData>()
        ]
