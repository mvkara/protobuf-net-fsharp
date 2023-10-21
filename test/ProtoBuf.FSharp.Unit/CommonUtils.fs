namespace ProtoBuf.FSharp.Unit

open System

type TestNameAttribute(name: string, dependentTypes: Type array) =
    inherit Attribute()
    new (name: string) = TestNameAttribute(name, [||])
    member __.Name = name
    member __.DependentTypeParamters = dependentTypes


module Roundtrip =
    open System.Buffers
    open Expecto
    open Expecto.Expect
    open FsCheck
    open ProtoBuf.Meta
    open ProtoBuf.FSharp

    // F# does not allow nulls although FsCheck tries to stress C# interoperability.
    // Disabling it here because this library is for wrapping F# types only.
    type DataGenerator =
        // static member Generate() : Arbitrary<string[]> =
        //     Gen.oneof ([ "One"; "Two"; "" ] |> List.map Gen.constant)
        //     |> Gen.listOf
        //     |> Gen.map List.toArray
        //     |> Arb.fromGen

        static member GenerateNonNullString() : Arbitrary<string> =
            Arb.Default.StringWithoutNullChars().Generator |> Gen.map (fun x -> x.Get) |> Gen.filter (box >> Operators.isNull >> not) |> Arb.fromGen


    let private fsCheckConfig = {
        FsCheckConfig.defaultConfig with
            maxTest = 1000
            arbitrary = [ typeof<DataGenerator> ]
    }


    let private prepareModel<'t> (otherDependentRecordTypes: Type[]) =
        let name = sprintf "Model for %A" typeof<'t>
        let model = RuntimeTypeModel.Create(name) |> Serialiser.registerTypeIntoModel<'t>
        if otherDependentRecordTypes <> null then
            for dependentRecordType in otherDependentRecordTypes do
                Serialiser.registerRuntimeTypeIntoModel dependentRecordType model |> ignore
        model.CompileInPlace()
        model

    let private roundtripSerialise<'t when 't : equality> (model: RuntimeTypeModel) (valueToTest: 't) =
        let cloned = model.DeepClone(valueToTest)
        equal (unbox cloned) valueToTest "Protobuf deep clone"

        let rtData =
            let bw = ArrayBufferWriter(512)
            model.Serialize(bw, valueToTest)
            model.Deserialize<'t>(bw.WrittenSpan, Unchecked.defaultof<'t>, null)
            // use ms = new MemoryStream()
            // Serialiser.serialise model ms typeToTest
            // ms.Seek(0L, SeekOrigin.Begin) |> ignore
            // Serialiser.deserialise<'t> model ms

        equal rtData valueToTest "ser/des yields different result"

    let buildProperty<'t when 't : equality> () =
        let struct (testName, otherDependentRecordTypes) =
            match typeof<'t>.GetCustomAttributes(typeof<TestNameAttribute>, true) with
            | [| :? TestNameAttribute as attr |] ->
                struct (sprintf "%s (type = %A)" attr.Name typeof<'t>, attr.DependentTypeParamters)
            | _ -> struct (sprintf "Roundtrip for %A" typeof<'t>, Array.empty)

        let model = prepareModel<'t> otherDependentRecordTypes
        testPropertyWithConfig fsCheckConfig testName (roundtripSerialise<'t> model)

    let testValue<'t when 't : equality> otherDependentRecordTypes (valueToTest: 't) =
        let model = prepareModel<'t> otherDependentRecordTypes
        roundtripSerialise model valueToTest
