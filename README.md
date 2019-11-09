# Protobuf-Net FSharp Wrapper #

This library is intended to provide a set of helper functions to allow F# types to be seriaised/deserialised using the protobuf-net library making it easier to use from a F# environment.

## Aims ##

- Support common F# types (Options, Records, DU)
- Leverage improvements/work done in protobuf-net library whilst adding F# specifc improvements on top (e.g. proto generation, gRpc, etc.)
- Make it friendly to use from an F# environment (e.g. no implicit nulls during deserialisation where F# compiler doesn't expect them).

## Supported/Non supported features ##

- Records
- Discriminated Unions
- Option fields on the above types
- Empty arrays and strings are populated across the whole object graph during deserialisation to avoid unexpected null's in an F# context.

Currently the non-supported features are mainly around the F# collections (Set, List, etc.). For the moment it is recommended to use arrays in your contract types to be serialised/deserialised. More work is required to support these.

## How to build/test ##

Using Net Core 3.0 or above:

```
dotnet tool restore
dotnet build
```

To run tests:

```
cd test/ProtoBuf.FSharpHelpers.Unit
dotnet run
```

## How to use ##

All methods are in the ProtoBuf.FSharp.ProtobufNetSerialiser module. They all require a previous RuntimeTypeModel
to be created and/or retrieved in advance. Detailed examples are found in the unit test projects.

A quick example of how to get the runtime model, and register both an existing record and DU type against it is as follows:

```
open ProtoBuf.FSharp

let model = 
    RuntimeTypeModel.Create() // OR RuntimeTypeModel.Default
    |> Serialiser.registerRecordIntoModel<RecordType> 
    |> Serialiser.registerUnionIntoModel<UnionType>
```

An example below of how to use the model to serialise and deserialise an object assuming the code above has been run:
    
```
let typeToTest = { RecordType.TestData = "TEST" }
use ms = new MemoryStream()
Serialiser.serialise model ms typeToTest
ms.Seek(0L, SeekOrigin.Begin) |> ignore
let rtData = Serialiser.deserialise<'t> model ms
```

## Issues ##

Any issues using this feel free to raise an issue on this repository.


            
