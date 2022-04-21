module FSharpWorkshopEnterpriseConsole
// For more information see https://aka.ms/fsharp-console-apps

open System
open FSharpWorkshopEnterprise.Implementation

[<EntryPoint>]
let main argv =
    let expected = System.Enum.Parse(typedefof<FiatCurrency>, "PLN")
    let balance =
        argv
        |> Array.map tryParseMoney

    printfn "Hello from F#"
    
    0