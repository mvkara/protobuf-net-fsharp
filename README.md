# Protobuf-Net FSharp Helper #

This library is intended to provide a set of helper functions to allow F# types to be seriaised/deserialised
using the protobuf-net library.

## How to use ##

All methods are in the ProtoBuf.FSharp.ProtobufNetSerialiser module. They all require a previous RuntimeTypeModel
to be created and/or retrieved in advance.

An example of how to get the runtime mode, and register both an existing record and DU type against it is as follows:

    let model = 
        RuntimeTypeModel.Create() // OR RuntimeTypeModel.Default
        |> ProtobufNetFSharp.registerRecordIntoModel<RecordType> 
        |> ProtobufNetFSharp.registerUnionIntoModel<UnionType>

An example below of how to use the model to serialise and deserialise an object assuming the code above has been run:
    
    let typeToTest = { RecordType.TestData = "TEST" }
    use ms = new MemoryStream()
    ProtobufNetFSharp.serialise model ms typeToTest
    ms.Seek(0L, SeekOrigin.Begin) |> ignore
    let rtData = ProtobufNetFSharp.deserialise<'t> model ms

## Collection Support ##

When using Protobuf-net with F# there are some issues when using collections:

- By default only the standard .NET collections are supported. F# collections (not being OO based) don't work by default.
- If a collection is serialised as an empty list it is deserialised as a Null on the other side.
  - This can play havoc with F# as it introduces nulls into an environment which does not expect them.
    An example would be if you are doing a structural equality on two records and two levels deep one array is a Null
    causing the equality to blow up.

This library adds supports for both by adding surrogate types to the model as required for any records/unions registered with the stndard ProtobufNetSerialiser:

- Arrays (Not nullable)
  - All arrays that are empty are deserialised as empty arrays rather than Nulls. In other words an array will never be deserialised as a Null.
- Lists (FSharpList<_>)
  - Full roundtrip serialisation support. Any nulls from serialisation will be treated as an empty list as expected.

If you need to serialise a collection by itself (not in a record/DU) or as part of a standard POJO simply
use the CollectionRegistration.registerCollectionWithReflectedTypeIntoModel function.

## Issues ##

Any issues using this feel free to raise an issue on this repository.


            
