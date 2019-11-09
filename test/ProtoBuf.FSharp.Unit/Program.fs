open System
open Expecto
open System.Diagnostics

[<EntryPoint>]
let main argv = Tests.runTestsInAssembly defaultConfig argv
