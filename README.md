# Protobuf-Net FSharp Helper #

This library is intended to provide a set of helper functions to allow F# types to be seriaised/deserialised
using the protobuf-net library.

## How to use ##

All methods are in the ProtoBuf.FSharp.ProtobufNetSerialiser module. They all require a previous RuntimeTypeModel
to be created and/or retrieved in advance.

An example of how to get the runtime mode, and register both an existing record and DU type against it is as follows:

    let model = 
    	RuntimeTypeModel.Create() // OR RuntimeTypeModel.Default
        |> ProtobufNetSerialiser.registerRecordIntoModel<RecordType> 
        |> ProtobufNetSerialiser.registerUnionIntoModel<UnionType>

An example below of how to use the model to serialise and deserialise an object assuming the code above has been run:
    
    let typeToTest = { RecordType.TestData = "TEST" }
    use ms = new MemoryStream()
    ProtobufNetSerialiser.serialise model ms typeToTest
    ms.Seek(0L, SeekOrigin.Begin) |> ignore
    let rtData = ProtobufNetSerialiser.deserialise<'t> model ms

## Issues ##

Any issues using this feel free to raise an issue on this repository.


            
