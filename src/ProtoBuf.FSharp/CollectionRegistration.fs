module ProtoBuf.FSharp.CollectionRegistration

open ProtoBuf.Meta
open ProtoBuf
open System

[<ProtoContract>]
type ListSurrogate<'t>() =
    [<ProtoMember(1)>]
    member val public List = Array.empty<'t> with get, set

    static member public To (l: 't list) =
        match box l with
        | null -> ListSurrogate(List = Array.empty<'t>)
        | _ when List.isEmpty l -> ListSurrogate(List = Array.empty<'t>)
        | _ -> ListSurrogate(List = (l |> Seq.toArray))

    static member public From (l: 't ListSurrogate) =
        match box l.List with
        | null -> []
        | _ -> ((l.List) |> List.ofSeq)

    static member public op_Implicit (l: 't list) : ListSurrogate<'t> = ListSurrogate<'t>.To(l)
    static member public op_Implicit (l: 't ListSurrogate) : 't list = ListSurrogate<'t>.From(l)

[<ProtoContract>]
type ArraySurrogate<'t>() =
    [<ProtoMember(1)>]
    member val public List = Array.empty<'t> with get, set

    static member public To (l: 't array) =
        match l with
        | null -> ArraySurrogate(List = Array.empty<'t>)
        | _ when Array.isEmpty l -> ArraySurrogate(List = Array.empty<'t>)
        | _ -> ArraySurrogate(List = (l |> Seq.toArray))

    static member public From (l: 't ArraySurrogate) =
        match box l.List with
        | null -> [||]
        | _ -> l.List

    static member public op_Implicit (l: 't array) : ArraySurrogate<'t> = ArraySurrogate<'t>.To(l)
    static member public op_Implicit (l: 't ArraySurrogate) : 't array = ArraySurrogate<'t>.From(l)

let public registerCollectionWithReflectedTypeIntoModel (model: RuntimeTypeModel) (listType: Type) =
    let registerFunc (surrogateType: Type) =
        let itemType = listType.GetGenericArguments().[0]
        let collectionSurrogateType = surrogateType.MakeGenericType(itemType)
        let t = model.Add(listType, false)
        t.IgnoreListHandling <- true // Bug: MetaType.ResolveListTypes does not respect IgnoreListHandling = true flag.
        t.SetSurrogate(collectionSurrogateType)
        model

    let surrogateTypeOpt =
        lazy
            if listType.IsGenericType
            then
                match (listType.GetGenericTypeDefinition()) with
                | x when x = (typedefof<list<_>>) -> typedefof<ListSurrogate<_>> |> Some
                | x when x = (typedefof<_ array>) -> typedefof<ArraySurrogate<_>> |> Some
                | _ -> None
            else None

    if (model.IsDefined(listType) || Option.isNone surrogateTypeOpt.Value)
    then model
    else registerFunc (surrogateTypeOpt.Value |> Option.get)

let public registerListTypeIntoModel<'t> (model: RuntimeTypeModel) =
   registerCollectionWithReflectedTypeIntoModel model typeof<'t list>