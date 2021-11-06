namespace ProtoBuf.FSharp.Surrogates

open System
open System.Collections
open System.Collections.Generic
open ProtoBuf
open ProtoBuf.Meta
open ProtoBuf.FSharp.ZeroValues


[<Struct; CLIMutable; ProtoContract>]
type Optional<'t> =
    { [<ProtoMember(1)>] HasValue: bool
      [<ProtoMember(2)>] Item: 't }

    static member op_Implicit (o: 't option) : Optional<'t> =
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

    static member op_Explicit (list: 't list) : ListSurrogate<'t> =
        if obj.ReferenceEquals(list, null) || List.isEmpty list then
            ListSurrogate(List = [| |])
        else
            ListSurrogate(List = List.toArray(list))

    static member op_Explicit (sur: 't ListSurrogate) : 't list =
        match sur.List with
        | null | [| |] -> List.empty
        | list when isApplicableTo typeof<'t> -> [ for el in list -> fixZeroValue el ]
        | list -> List.ofArray list


[<Struct; ProtoContract>]
type SetSurrogate<'t when 't : comparison> =
    [<ProtoMember(1)>]
    val mutable List : 't[]

    static member op_Explicit (set: Set<'t>) : SetSurrogate<'t> =
        if obj.ReferenceEquals(set, null) || Set.isEmpty set then
            SetSurrogate(List = [| |])
        else
            SetSurrogate(List = Set.toArray(set))

    static member op_Explicit (sur: SetSurrogate<'t>) : Set<'t> =
        match sur.List with
        | null | [| |] -> Set.empty
        | list when isApplicableTo typeof<'t> -> seq { for el in list -> fixZeroValue el } |> Set.ofSeq
        | list -> Set.ofArray list


[<AbstractClass>]
type MutMap<'k, 'v when 'k : comparison> () =
    member val Map = Map.empty<'k, 'v> with get, set

    abstract InternalAdd : 'k * 'v -> unit

    interface IEnumerable with
        member this.GetEnumerator() = (this.Map :> IEnumerable).GetEnumerator()

    interface IEnumerable<KeyValuePair<'k, 'v>> with
        member this.GetEnumerator() = (this.Map :> IEnumerable<_>).GetEnumerator()

    interface ICollection<KeyValuePair<'k, 'v>> with
        member __.Clear() = NotImplementedException "ICollection<_>.Clear" |> raise
        member __.Contains _ = NotImplementedException "ICollection<_>.Contains" |> raise
        member __.Remove _ = NotImplementedException "ICollection<_>.Remove" |> raise
        member __.CopyTo(_, _) = NotImplementedException "ICollection<_>.CopyTo" |> raise
        member __.IsReadOnly = false
        member this.Add kv = this.InternalAdd(kv.Key, kv.Value)
        member this.Count = this.Map.Count

    interface IDictionary<'k, 'v> with
        member __.ContainsKey _ = NotImplementedException "IDictionary<_,_>.ContainsKey" |> raise
        member __.Remove(_ : 'k) = NotImplementedException "IDictionary<_,_>.Remove" |> raise
        member this.Keys = (this.Map :> IDictionary<_, _>).Keys
        member this.Values = (this.Map :> IDictionary<_, _>).Values
        member this.TryGetValue(k, refV) = this.Map.TryGetValue(k, &refV)
        member this.Add(k, v) = this.InternalAdd(k, v)
        member this.Item
            with get k = this.Map.[k]
            and set k v = this.InternalAdd(k, v)

[<Struct; ProtoContract>]
type MapSurrogate<'k, 'v when 'k : comparison> =
    [<ProtoMember(1); ProtoMap()>]
    val mutable Map : IDictionary<'k, 'v>

    [<ProtoBeforeDeserialization>]
    member this.BeforeDeserialization () =
        this.Map <-
            match isApplicableTo typeof<'k>, isApplicableTo typeof<'v> with
            | false, false -> { new MutMap<'k, 'v>() with member __.InternalAdd(k, v) = base.Map <- Map.add k v base.Map }
            | true, false -> { new MutMap<'k, 'v>() with member __.InternalAdd(k, v) = base.Map <- Map.add (fixZeroValue k) v base.Map }
            | false, true -> { new MutMap<'k, 'v>() with member __.InternalAdd(k, v) = base.Map <- Map.add k (fixZeroValue v) base.Map }
            | true, true -> { new MutMap<'k, 'v>() with member __.InternalAdd(k, v) = base.Map <- Map.add (fixZeroValue k) (fixZeroValue v) base.Map }

    static member op_Explicit (map: Map<'k, 'v>) : MapSurrogate<'k, 'v> =
        MapSurrogate(Map = map)

    static member op_Explicit (sur: MapSurrogate<'k, 'v>) : Map<'k, 'v> =
        (sur.Map :?> MutMap<'k, 'v>).Map
