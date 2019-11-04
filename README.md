# Protobuf-Net FSharp Helper #

This library is intended to provide a set of helper functions to allow F# types to be seriaised/deserialised
using the protobuf-net library.

## Aims ##

- Support common F# types (Options, Records, DU)
- Leverage improvements/work done in protobuf-net library whilst adding F# specifc improvements on top (e.g. proto generation, gRpc, etc.)
- Serialisation/deserialisation roundtrips should not result in Nulls for non-optional fields (e.g. empty strings, collections). This allows easier structural comparison of deserialised objects.

## Supported/Non supported features ##

- Records
- Discriminated Unions
- Option fields on the above types

Currently the non-supported features are mainly around the F# collections (Set, List, etc.). More work is required to support these.

## How to build ##

Using Net Core 3.0 or above:

```
dotnet tool restore
dotnet build
```

## How to use ##

All methods are in the ProtoBuf.FSharp.ProtobufNetSerialiser module. They all require a previous RuntimeTypeModel
to be created and/or retrieved in advance.

An example of how to get the runtime mode, and register both an existing record and DU type against it is as follows:

    let model = 
    	RuntimeTypeModel.Create() // OR RuntimeTypeModel.Default
        |> ProtobufNetSerialiser.registerRecordIntoModel<RecordType> 
        |> ProtobufNetSerialiser.registerUnionIntoModel<UnionType>

An example below of how to use the model to serialise and deserialise an object assuming the code above has been run:
    
```
let typeToTest = { RecordType.TestData = "TEST" }
use ms = new MemoryStream()
ProtobufNetSerialiser.serialise model ms typeToTest
ms.Seek(0L, SeekOrigin.Begin) |> ignore
let rtData = ProtobufNetSerialiser.deserialise<'t> model ms
```

## Issues ##

Any issues using this feel free to raise an issue on this repository.


            
