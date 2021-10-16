module ProtoBuf.FSharp.CollectionRegistration

open ProtoBuf.Meta
open System
open System.Collections.Generic


let internal collectionTypes =
    HashSet (seq { typedefof<_ list>; typedefof<Set<_>>; typedefof<Map<_, _>> })


let registerCollectionWithReflectedTypeIntoModel (model: RuntimeTypeModel) (listType: Type) =
    if model.IsDefined(listType) then
        ()
    elif listType.IsGenericType then
        if listType.GetGenericTypeDefinition() |> collectionTypes.Contains then
            let t = model.Add(listType, false)
            t.IgnoreListHandling <- true // Bug: MetaType.ResolveListTypes does not respect IgnoreListHandling = true flag.
            CodeGen.getSurrogate listType |> t.SetSurrogate
    elif listType.IsArray then // Doesn't seem to actually work
        let itemType = listType.GetElementType()
        let collectionSurrogateType = typedefof<Surrogates.ArraySurrogate<_>>.MakeGenericType(itemType)
        let t = model.Add(listType, false)
        t.IgnoreListHandling <- true
        t.SetSurrogate(collectionSurrogateType)


let registerListTypeIntoModel<'t> (model: RuntimeTypeModel) =
   registerCollectionWithReflectedTypeIntoModel model typeof<'t list>
   model
