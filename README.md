# Protobuf-Net FSharp Wrapper #

This library is intended to provide a set of helper functions to allow F# types to be seriaised/deserialised using the protobuf-net library making it easier to use from a F# environment.

[![NuGet Badge](http://img.shields.io/nuget/v/protobuf-net-fsharp.svg?style=flat)](https://www.nuget.org/packages/protobuf-net-fsharp)
![CI Badge](https://github.com/mvkara/protobuf-net-fsharp/actions/workflows/test.yml/badge.svg)

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

Using Net Core 3.1 or above:

```bash
dotnet tool restore
dotnet build
```

To run tests:

```bash
dotnet test
```

## How to use ##

All methods are in the ProtoBuf.FSharp.Serialiser module. They all require a previous RuntimeTypeModel
to be created and/or retrieved in advance. Detailed examples are found in the unit test projects.

A quick example of how to get the runtime model, and register both an existing record and DU type against it is as follows:

```fsharp
open ProtoBuf.FSharp

let model = 
    RuntimeTypeModel.Create("Model") // OR RuntimeTypeModel.Default
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

## Issues ##

Any issues using this feel free to raise an issue on this repository.
