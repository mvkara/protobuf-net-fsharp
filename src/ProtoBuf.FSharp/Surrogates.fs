namespace ProtoBuf.FSharp.Surrogates

open System.Collections.Generic
open ProtoBuf
open ProtoBuf.Meta
open ProtoBuf.FSharp.ZeroValues

[<Struct; CLIMutable; ProtoContract>]
type Optional<'t> =
    { [<ProtoMember(1)>] HasValue: bool
      [<ProtoMember(2)>] Item: 't }

    static member op_Implicit (o: 't option) =
        match o with
        | Some(o) -> { Item = o; HasValue = true }
        | None -> { HasValue = false; Item = Unchecked.defaultof<_> }

    static member op_Implicit (w: Optional<'t>) : 't option =
        match w.HasValue with
        | false -> None
        | true when isApplicableTo typeof<'t> -> w.Item |> fixZeroValue |> Some
        | true -> Some w.Item

    static member RegisterIntoModel (model : RuntimeTypeModel) =
        let surrogateModelType = model.Add(typeof<Optional<'t>>, true)
        surrogateModelType.Name <- "Optional" + typeof<'t>.Name

[<Struct; ProtoContract>]
type ListSurrogate<'t> =
    [<ProtoMember(1)>]
    val mutable List : 't[]

    static member op_Explicit (list: 't list) =
        match box list with
        | null -> ListSurrogate(List = [| |])
        | _ when List.isEmpty list -> ListSurrogate(List = [| |])
        | _ -> ListSurrogate(List = List.toArray(list))

    static member op_Explicit (sur: 't ListSurrogate) =
        match sur.List with
        | null | [| |] -> List.empty
        | list when isApplicableTo typeof<'t> -> [ for el in list -> fixZeroValue el ]
        | list -> List.ofArray list

[<Struct; ProtoContract>]
type SetSurrogate<'t when 't : comparison> =
    [<ProtoMember(1)>]
    val mutable List : 't[]

    static member op_Explicit (set: Set<'t>) =
        match box set with
        | null -> SetSurrogate(List = [| |])
        | _ when Set.isEmpty set -> SetSurrogate(List = [| |])
        | _ -> SetSurrogate(List = Set.toArray(set))

    static member op_Explicit (sur: SetSurrogate<'t>) =
        match sur.List with
        | null | [| |] -> Set.empty
        | list when isApplicableTo typeof<'t> -> seq { for el in list -> fixZeroValue el } |> Set.ofSeq
        | list -> Set.ofArray list

[<Struct; ProtoContract>]
type MapSurrogate<'k, 'v when 'k : comparison> =
    [<ProtoMember(1)>]
    val mutable List : KeyValuePair<'k, 'v>[]

    static member op_Explicit (map: Map<'k, 'v>) =
        match box map with
        | null -> MapSurrogate(List = [| |])
        | _ when Map.isEmpty map -> MapSurrogate(List = [| |])
        | _ -> MapSurrogate(List = [| for kv in map -> kv |])

    static member op_Explicit (sur: MapSurrogate<'k, 'v>) =
        match sur.List with
        | null | [| |] -> Map.empty
        | list when isApplicableTo typeof<'k> && isApplicableTo typeof<'v> ->
            seq { for KeyValue(k, v) in list -> (fixZeroValue k, fixZeroValue v) } |> Map.ofSeq
        | list when isApplicableTo typeof<'k> ->
            seq { for KeyValue(k, v) in list -> (fixZeroValue k, v) } |> Map.ofSeq
        | list when isApplicableTo typeof<'v> ->
            seq { for KeyValue(k, v) in list -> (k, fixZeroValue v) } |> Map.ofSeq
        | list ->
            seq { for KeyValue(k, v) in list -> (k, v) } |> Map.ofSeq

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
        match l.List with
        | null -> [||]
        | _ -> l.List

    static member public op_Implicit (l: 't array) : ArraySurrogate<'t> = ArraySurrogate<'t>.To(l)
    static member public op_Implicit (l: 't ArraySurrogate) : 't array = ArraySurrogate<'t>.From(l)
