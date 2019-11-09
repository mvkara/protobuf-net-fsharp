namespace ProtoBuf.FSharp.Unit

open System

type TestNameAttribute(name: string) = 
    inherit Attribute()
    member __.Name = name