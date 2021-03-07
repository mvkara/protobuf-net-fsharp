namespace ProtoBuf.FSharp

open System
open System.Collections.Concurrent
open System.Reflection

module public ZeroValueFactoryMethods =
    let getEmptyString() : string = String.Empty
    let getEmptyFSharpList<'t>() : 't list = List.empty
    let getEmptyArray<'t>() : 't array = Array.empty
    let getEmptySet<'t when 't : comparison>() : Set<'t> = Set.empty
    let getEmptyMap<'t, 'tv when 't : comparison>() : Map<'t, 'tv> = Map.empty

module internal ZeroValues =

    open ZeroValueFactoryMethods

    let private zeroValueFieldSetters = ConcurrentDictionary<Type, MethodInfo>()

    let getZeroValueMethodInfoOpt (fieldType: Type) =
        /// Creates the zero value for supported types that we know of.
        let createZeroValueMethodInfoSetter() =
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

        match zeroValueFieldSetters.TryGetValue(fieldType) with
        | (true, zeroValue) -> Some zeroValue
        | (false, _) ->
            match createZeroValueMethodInfoSetter() with
            | Some(zeroValue) ->
                zeroValueFieldSetters.[fieldType] <- zeroValue
                Some zeroValue
            | None -> None

    type [<Struct>] internal FieldWithZeroValueSetMethod = { FieldInfo: FieldInfo; ZeroValueMethod: MethodInfo }

    let calculateApplicableFields (fields: FieldInfo[]) = [|
        for fi in fields do
            match getZeroValueMethodInfoOpt fi.FieldType with
            | None -> ()
            | Some getZeroValueMethod ->  { FieldInfo = fi; ZeroValueMethod = getZeroValueMethod }
    |]