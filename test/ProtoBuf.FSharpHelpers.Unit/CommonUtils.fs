namespace ProtoBuf.FSharpHelpers.Unit

open System

type TestNameAttribute(name: string) = 
    inherit Attribute()
    member __.Name = name