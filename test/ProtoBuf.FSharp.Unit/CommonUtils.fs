namespace ProtoBuf.FSharp.Unit

open System

type TestNameAttribute(name: string, dependentTypes: Type array) = 
    inherit Attribute()
    new (name: string) = TestNameAttribute(name, [||])
    member __.Name = name
    member __.DependentTypeParamters = dependentTypes