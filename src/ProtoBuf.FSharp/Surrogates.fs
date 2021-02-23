module private ProtoBuf.FSharp.Surrogates


type [<CLIMutable>] Optional<'t> =
    { HasValue: bool
      Item: 't }
    static member op_Implicit (w: Optional<'t>) : 't option =
        if w.HasValue then Some w.Item else None
    static member op_Implicit (o: 't option) =
        match o with
        | Some(o) -> { Item = o; HasValue = true }
        | None -> { HasValue = false; Item = Unchecked.defaultof<_> }
