# Protobuf-Net FSharp Wrapper #

This library is intended to provide a set of helper functions to allow F# types to be seriaised/deserialised using the protobuf-net library making it easier to use from a F# environment.

[![NuGet Badge](http://img.shields.io/nuget/v/protobuf-net-fsharp.svg?style=flat)](https://www.nuget.org/packages/protobuf-net-fsharp)

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

All methods are in the ProtoBuf.FSharp.Serialiser module. They all require a previous RuntimeTypeModel
to be created and/or retrieved in advance. Detailed examples are found in the unit test projects.

A quick example of how to get the runtime model, and register both an existing record and DU type against it is as follows:

```fsharp
open ProtoBuf.FSharp

let model = 
    RuntimeTypeModel.Create() // OR RuntimeTypeModel.Default
    |> Serialiser.registerRecordIntoModel<RecordType> 
    |> Serialiser.registerUnionIntoModel<UnionType>
```

An example below of how to use the model to serialise and deserialise an object assuming the code above has been run:
    
```fsharp
let typeToTest = { RecordType.TestData = "TEST" }
use ms = new MemoryStream()
Serialiser.serialise model ms typeToTest
ms.Seek(0L, SeekOrigin.Begin) |> ignore
let rtData = Serialiser.deserialise<'t> model ms
```

## Details

### Option Serialisation

By default any usages of F# options in your registered records and/or unions are also registered into the Protobuf model.

An example for the ```Option<string>``` type the message as described in a proto file would be:

```protobuf
message OptionalString {
   bool HasValue = 1;
   string Item = 2;
}
```

The default behaviour for the name of the protobuf message type (in this case OptionalString) are:
  - All option types are prefixed with "Optional".
  - The suffix for the name is the parameter type's "Name" property.

You can customise the suffix by registering the option type yourself but note this needs to be done before registering any types that use this type else the default behaviour of this library takes precedence.

**Note**: As per the [Protobuf spec](https://developers.google.com/protocol-buffers/docs/proto3#default) default values can't be distinguished from missing values. The HasValue property distinguishes "None" cases from a (Some defaultValue) case.

### Default Value Serialisation

Because default values are not serialised by Protobuf (empty strings, lists, etc) protobuf-net at time of writing when deserialising these objects often does not populate these fields leaving them with CLR null values. This breaks the F# assumption that fields on records and unions unless specified are not nullable and inhibits roundtrip serialisation. In other words serialising then deserialising an F# type does not preserve these values. This creates problems in an F# context especially with records and unions where constructors can't be overriden easily to prepopulate these types and the environment doesn't expect nulls to be definable on these types in normal use. 

This library adds a dynamic factory for registered types that populates the default values upon deserialsation for types.

As an example the record
```fsharp
type ExampleRecord = { TestOne: string; TestTwo: int array }
let objectToSerialise = { TestOne = String.Empty; TestTwo = Array.empty }
```
will now deserialise as 

```fsharp
{ TestOne = String.Empty; TestTwo = Array.empty }
```

vs the default protobuf-net behaviour at time of writing

```fsharp
{ TestOne = null; TestTwo = null }
```

## Collection Support ##

**NOTE: This requires a PR to the protobuf-net library to enable custom surrogates. Until this is approved this feature branch is a POC only (https://github.com/mgravell/protobuf-net/pull/336)**

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
