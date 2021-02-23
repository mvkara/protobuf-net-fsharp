module ProtoBuf.FSharp.ZeroValues

open System
open System.Collections.Concurrent
open System.Reflection


let GetEmptyString() : string = String.Empty
let GetEmptyFSharpList<'t>() : 't list = List.empty
let GetEmptyArray<'t>() : 't array = Array.empty
let GetEmptySet<'t when 't : comparison>() : Set<'t> = Set.empty
let GetEmptyMap<'t, 'tv when 't : comparison>() : Map<'t, 'tv> = Map.empty


let private zeroValues = ConcurrentDictionary<Type, MethodInfo>()

let calculateIfApplicable (fieldType: Type) =
    /// Creates the zero value for supported types that we know of.
    let createZeroValue() =
        if fieldType = typeof<string> then
            MethodHelpers.getMethodInfo <@ GetEmptyString @> [| |] |> Some
        elif fieldType.IsGenericType && fieldType.GetGenericTypeDefinition() = typedefof<_ list> then
            MethodHelpers.getMethodInfo <@ GetEmptyFSharpList @> fieldType.GenericTypeArguments |> Some
        elif fieldType.IsGenericType && fieldType.GetGenericTypeDefinition() = typedefof<Set<_>> then
            MethodHelpers.getMethodInfo <@ GetEmptySet @> fieldType.GenericTypeArguments |> Some
        elif fieldType.IsGenericType && fieldType.GetGenericTypeDefinition() = typedefof<Map<_, _>> then
            MethodHelpers.getMethodInfo <@ GetEmptyMap @> fieldType.GenericTypeArguments |> Some
        elif fieldType.IsArray then
            MethodHelpers.getMethodInfo <@ GetEmptyArray @> [| fieldType.GetElementType() |] |> Some
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
