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
        if obj.ReferenceEquals(list, null) || List.isEmpty list then
            ListSurrogate(List = [| |])
        else
            ListSurrogate(List = List.toArray(list))

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
        if obj.ReferenceEquals(set, null) || Set.isEmpty set then
            SetSurrogate(List = [| |])
        else
            SetSurrogate(List = Set.toArray(set))

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
        if obj.ReferenceEquals(map, null) || Map.isEmpty map then
            MapSurrogate(List = [| |])
        else
            MapSurrogate(List = [| for kv in map -> kv |])

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
