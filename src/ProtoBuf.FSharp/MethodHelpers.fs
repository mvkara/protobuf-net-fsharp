module private ProtoBuf.FSharp.MethodHelpers

open System
open Microsoft.FSharp.Quotations.Patterns
open System.Reflection


/// Allows you to get the nameof a method in older F# versions
let private nameOfQuotation methodQuotation =
    match methodQuotation with
    | Lambda(_, Call(_, mi, _))
    | Lambda(_, Lambda(_, Call(_, mi, _))) -> mi.DeclaringType, mi.Name
    | x -> failwithf "Not supported %A" x

let private bindingFlagsToUse = BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static

let getMethodInfo quotation (typeParameters: Type array) =
    let (declaringType, nameOfMethod) = nameOfQuotation quotation
    match typeParameters.Length with
    | 0 -> declaringType.GetMethod(nameOfMethod, bindingFlagsToUse)
    | _ -> declaringType.GetMethod(nameOfMethod, bindingFlagsToUse).MakeGenericMethod(typeParameters)
