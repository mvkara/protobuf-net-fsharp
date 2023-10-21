namespace ProtoBuf.FSharp.Unit

open Expecto

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

[<Struct; TestName("Struct record with inner complex types", [| typeof<InnerNestedRecordWithCollections> |])>]
type StructRecordWithNestedTypes = {
    DataCollection: InnerNestedRecordWithCollections array
    Data: InnerNestedRecordWithCollections
}

[<Struct; TestName("Struct record with struct collection type", [| typeof<StructRecordWithCollectionTestCases>; typeof<InnerNestedRecordWithCollections> |])>]
type StructRecordWithNestedStructCollectionTypes = {
    StructDataCollection: StructRecordWithCollectionTestCases array
    Data: InnerNestedRecordWithCollections
}

[<Struct; TestName("Struct record with 2 generic arguments")>]
type StructWith2GenericArs<'t, 'r> = {
    Count : int
    Data : 't[]
    Data2 : 'r
}

module TestRecordRoundtrip =
    let manualTestCases = [
        testCase "Can serialise empty array, string and option" <| fun () -> Roundtrip.testValue [||] { One = ""; Two = 1; Three = [||]; Four = None }
        testCase "Can serialise option containing value" <| fun () -> Roundtrip.testValue [||] { One = ""; Two = 1; Three = [||]; Four = Some "TEST" }
        testCase "Can serialise string, array and option containing value" <| fun () -> Roundtrip.testValue [||] { One = "TEST"; Two = 1; Three = [| "TEST1" |]; Four = Some "TEST" }
    ]

    [<Tests>]
    let test() =
        testList "Record Test Cases" [
            testList "Manual" manualTestCases
            Roundtrip.buildProperty<TestRecordOne>()
            Roundtrip.buildProperty<TestRecordTwo>()
            Roundtrip.buildProperty<TestRecordThree>()
            Roundtrip.buildProperty<TestRecordFour>()
            Roundtrip.buildProperty<TestRecordFive>()
            Roundtrip.buildProperty<TestRecordSix>()
            Roundtrip.buildProperty<TestRecordSeven>()
            Roundtrip.buildProperty<NestedRecordWithZeroValues>()
            Roundtrip.buildProperty<StructRecordWithCollectionTestCases>()
            Roundtrip.buildProperty<StructRecordWithNestedTypes>()
            Roundtrip.buildProperty<StructRecordWithNestedStructCollectionTypes>()
            Roundtrip.buildProperty<StructWith2GenericArs<string, int>>()
            Roundtrip.buildProperty<StructWith2GenericArs<string, string[]>>()
            Roundtrip.buildProperty<StructWith2GenericArs<int, string[] option>>()
        ]
