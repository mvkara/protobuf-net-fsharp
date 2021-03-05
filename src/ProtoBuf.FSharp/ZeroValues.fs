module ProtoBuf.FSharp.ZeroValues

open System
open System.Collections.Concurrent
open System.Reflection


let getEmptyString() : string = String.Empty

let getEmptyFSharpList<'t>() : 't list = List.empty

let getEmptyArray<'t>() : 't array = Array.empty

let getEmptySet<'t when 't : comparison>() : Set<'t> = Set.empty

let getEmptyMap<'t, 'tv when 't : comparison>() : Map<'t, 'tv> = Map.empty


let private zeroValues = ConcurrentDictionary<Type, MethodInfo>()

let calculateIfApplicable (fieldType: Type) =
    /// Creates the zero value for supported types that we know of.
    let createZeroValue() =
        if fieldType = typeof<string> then
            MethodHelpers.getMethodInfo <@ getEmptyString @> [| |] |> Some
        elif fieldType.IsGenericType && fieldType.GetGenericTypeDefinition() = typedefof<_ list> then
            MethodHelpers.getMethodInfo <@ getEmptyFSharpList @> fieldType.GenericTypeArguments |> Some
        elif fieldType.IsGenericType && fieldType.GetGenericTypeDefinition() = typedefof<Set<_>> then
            MethodHelpers.getMethodInfo <@ getEmptySet @> fieldType.GenericTypeArguments |> Some
        elif fieldType.IsGenericType && fieldType.GetGenericTypeDefinition() = typedefof<Map<_, _>> then
            MethodHelpers.getMethodInfo <@ getEmptyMap @> fieldType.GenericTypeArguments |> Some
        elif fieldType.IsArray then
            MethodHelpers.getMethodInfo <@ getEmptyArray @> [| fieldType.GetElementType() |] |> Some
        else None

    match zeroValues.TryGetValue(fieldType) with
    | (true, zeroValue) -> Some zeroValue
    | (false, _) ->
        match createZeroValue() with
        | Some(zeroValue) ->
            zeroValues.[fieldType] <- zeroValue
            Some zeroValue
        | None -> None


let calculateApplicableFields (fields: FieldInfo[]) =
    [|  for fi in fields do
            match calculateIfApplicable fi.FieldType with
                | None -> ()
                | Some zeroValue -> yield struct (fi, zeroValue)
    |]
