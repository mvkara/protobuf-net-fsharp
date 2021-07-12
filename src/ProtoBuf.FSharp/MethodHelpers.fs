module internal ProtoBuf.FSharp.MethodHelpers

open System
open Microsoft.FSharp.Quotations.Patterns
open System.Reflection

/// Allows you to get the nameof a method in older F# versions
let nameOfQuotation methodQuotation =
    match methodQuotation with
    | Lambda(_, Call(_, mi, _))
    | Lambda(_, Lambda(_, Call(_, mi, _))) -> mi.DeclaringType, mi.Name
    | FieldGet(_, fi) -> (fi.DeclaringType, fi.Name)
    | x -> failwithf "Not supported %A" x

let bindingFlagsToUse = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static

let getMethodInfo quotation (typeParameters: Type array) =
    let (declaringType, nameOfMethod) = nameOfQuotation quotation
    match typeParameters.Length with
    | 0 -> declaringType.GetMethod(nameOfMethod, bindingFlagsToUse)
    | _ -> declaringType.GetMethod(nameOfMethod, bindingFlagsToUse).MakeGenericMethod(typeParameters)

let getPropertyInfo quotation =
    let (declaringType, nameOfMethod) = nameOfQuotation quotation
    declaringType.GetProperty(nameOfMethod, bindingFlagsToUse)

[<RequireQualifiedAccess>]
type MethodType = 
    | MethodInfo of MethodInfo
    | PropertyInfo of PropertyInfo
    | FieldInfo of FieldInfo
    | NewArray of elementType : Type

let getFetchFunc methodQuotation (typeParameters: Type array) =
    match methodQuotation with
    | Call(_, mi, _) when Array.isEmpty typeParameters -> MethodType.MethodInfo mi
    | Call(_, mi, _) -> mi.GetGenericMethodDefinition().MakeGenericMethod(typeParameters) |> MethodType.MethodInfo
    | Lambda(_, Call(_, mi, _))
    | Lambda(_, Lambda(_, Call(_, mi, _))) -> MethodType.MethodInfo (mi.MakeGenericMethod(typeParameters))
    | FieldGet(_, fi) -> MethodType.FieldInfo fi
    | x -> failwithf "Not supported %A" x