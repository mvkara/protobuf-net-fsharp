namespace ProtoBuf.FSharp

open System
open System.Collections.Concurrent
open System.Reflection

module ZeroValues =

    type internal FieldWithZeroValueSetMethod = { FieldInfo: FieldInfo; ZeroValueMethod: MethodHelpers.MethodType }

    let private zeroValueFieldSetters = ConcurrentDictionary<Type, MethodHelpers.MethodType>()

    let internal getZeroValueMethodInfoOpt (fieldType: Type) =
        /// Creates the zero value for supported types that we know of.
        let createZeroValueMethodInfoSetter() =
            if fieldType = typeof<string> then
                MethodHelpers.getFetchFunc <@ String.Empty @> [| |] |> Some
            elif fieldType.IsGenericType && fieldType.GetGenericTypeDefinition() = typedefof<_ list> then
                MethodHelpers.getFetchFunc <@ List.empty @> fieldType.GenericTypeArguments |> Some
            elif fieldType.IsGenericType && fieldType.GetGenericTypeDefinition() = typedefof<Set<_>> then
                MethodHelpers.getFetchFunc <@ Set.empty @> fieldType.GenericTypeArguments |> Some
            elif fieldType.IsGenericType && fieldType.GetGenericTypeDefinition() = typedefof<Map<_, _>> then
                MethodHelpers.getFetchFunc <@ Map.empty @> fieldType.GenericTypeArguments |> Some
            elif fieldType.IsArray then
                fieldType.GetElementType() |> MethodHelpers.MethodType.NewArray |> Some
            else None

        match zeroValueFieldSetters.TryGetValue(fieldType) with
        | (true, zeroValue) -> Some zeroValue
        | (false, _) ->
            match createZeroValueMethodInfoSetter() with
            | Some(zeroValue) ->
                zeroValueFieldSetters.[fieldType] <- zeroValue
                Some zeroValue
            | None -> None

    let internal calculateApplicableFields (fields: FieldInfo[]) = [|
        for fi in fields do
            match getZeroValueMethodInfoOpt fi.FieldType with
            | None -> ()
            | Some getZeroValueMethod ->  { FieldInfo = fi; ZeroValueMethod = getZeroValueMethod }
    |]

    let isApplicableTo (t : Type) =
        getZeroValueMethodInfoOpt t |> Option.isSome

    let getZeroValue<'t> () : 't =
        match getZeroValueMethodInfoOpt typeof<'t> with
        | Some (MethodHelpers.MethodType.NewArray elType) ->
            System.Array.CreateInstance(elType, 0) |> unbox
        | Some (MethodHelpers.MethodType.MethodInfo mi) ->
            mi.Invoke(null, null) |> unbox
        | Some (MethodHelpers.MethodType.FieldInfo fi) ->
            fi.GetValue(null) |> unbox
        | Some (MethodHelpers.MethodType.PropertyInfo pi) ->
            pi.GetValue(null) |> unbox
        | None ->
            sprintf "No zero value for %A" typeof<'t> |> NotImplementedException |> raise

    let internal fixZeroValue<'t> (x : 't) =
        match box x with
        | null -> getZeroValue<'t> ()
        | _ -> x