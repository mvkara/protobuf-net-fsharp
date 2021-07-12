namespace ProtoBuf.FSharp.Surrogates

open ProtoBuf.FSharp.ZeroValues

type [<CLIMutable>] Optional<'t> =
    { HasValue: bool
      Item: 't }

    static member op_Implicit (o: 't option) =
        match o with
        | Some(o) -> { Item = o; HasValue = true }
        | None -> { HasValue = false; Item = Unchecked.defaultof<_> }

    static member op_Implicit (w: Optional<'t>) : 't option =
        match w.HasValue with
        | false -> None
        | true when isApplicableTo typeof<'t> -> w.Item |> fixZeroValue |> Some
        | true -> Some w.Item
