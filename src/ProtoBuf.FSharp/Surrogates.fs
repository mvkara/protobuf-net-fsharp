namespace ProtoBuf.FSharp.Surrogates

open ProtoBuf
open ProtoBuf.Meta
open ProtoBuf.FSharp.ZeroValues

[<Struct; CLIMutable; ProtoContract()>]
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
