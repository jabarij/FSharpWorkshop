module FSharpWorkshop.Implementation.Finances.Parsing

open System
open System.Globalization
open Microsoft.FSharp.Core
open FSharpWorkshop.Implementation.ActivePatterns

[<Literal>]
let CurrencyRegexPattern = "(?<currency>[a-zA-Z]{3,4})"

[<Literal>]
let MoneyRegexPattern = "(?<amount>-?\d+(,\d+)?)\s(?<currency>[a-zA-Z]{3,4})"

let tryParseDecimal (provider: IFormatProvider) (str: string): decimal option =
    match Decimal.TryParse(str, NumberStyles.Number, provider) with
    | true, value -> Some value
    | false, _ -> None

let tryParseKnownCurrency (code: string): Currency option =
    match code with
    | "USD" -> Some USD
    | "PLN" -> Some PLN
    | "EUR" -> Some EUR
    | "GBP" -> Some GBP
    | _ -> None
    
let tryParseCurrency (str: string): Currency option =
    let _optionToResult fNone = function
        | Some value -> Ok value
        | None -> Error (fNone())
    let _resultToOption = function
        | Ok value -> Some value
        | Error _ -> None
    let result =
        match str with
        | null | "" -> None
        | TryRegexMatch CurrencyRegexPattern regexResults ->
            regexResults
            |> Map.tryFind "currency"
            |> Option.bind (fun code ->
                code
                |> tryParseKnownCurrency
                |> Option.orElseWith (fun () -> Some (OtherCurrency code)))
        | _ -> None
    result
    
let tryParseMoney (provider: IFormatProvider) (str: string): Money option =
    match str with
    | null | "" -> None
    | TryRegexMatch MoneyRegexPattern matches ->
        let amountResult =
            matches
            |> Map.tryFind "amount"
            |> Option.bind (tryParseDecimal provider)
        let currencyResult =
            matches
            |> Map.tryFind "currency"
            |> Option.bind tryParseCurrency
        match amountResult, currencyResult with
        | Some amount, Some currency -> Some { Amount = amount; Currency = currency }
        | _ -> None
    | _ -> None

let parseDecimal (provider: IFormatProvider) (str: string): Result<decimal, string> =
    match Decimal.TryParse(str, NumberStyles.Number, provider) with
    | true, value -> Ok value
    | false, _ -> Error $"Could not parse decimal: %s{str}"

let parseKnownCurrency (code: string): Result<Currency, string> =
    match code with
    | null | "" -> Error "Null or empty string"
    | "USD" -> Ok USD
    | "PLN" -> Ok PLN
    | "EUR" -> Ok EUR
    | "GBP" -> Ok GBP
    | _ -> Error $"Invalid or unknown currency code: %s{code}"

let parseCurrency (str: string): Result<Currency, string> =
    let _optionToResult fNone = function
        | Some value -> Ok value
        | None -> Error (fNone())
    let _resultToOption = function
        | Ok value -> Some value
        | Error _ -> None
    let result =
        match str with
        | null | "" -> Error "Null or empty string"
        | TryRegexMatch CurrencyRegexPattern regexResults ->
            let currencyCode =
                regexResults
                |> Map.tryFind "currency"
            let currency =
                currencyCode
                |> Option.map (fun code ->
                    code
                    |> parseKnownCurrency
                    |> _resultToOption
                    |> Option.defaultWith (fun () -> OtherCurrency code)
                    |> Ok)
                |> Option.defaultWith (fun () -> Error "Could not extract currency code")
            currency
        | _ -> Error $"Unknown currency: %s{str}"
    result
    
let parseMoney (provider: IFormatProvider) (str: string): Result<Money, string> =
    match str with
    | null | "" -> Error "Null or empty string"
    | TryRegexMatch MoneyRegexPattern matches ->
        let amountResult =
            matches
            |> Map.tryFind "amount"
            |> Option.map (parseDecimal provider)
            |> Option.defaultWith (fun () -> Error "Could not extract amount")
        let currencyResult =
            matches
            |> Map.tryFind "currency"
            |> Option.map parseCurrency
            |> Option.defaultWith (fun () -> Error "Could not extract currency")
        match amountResult, currencyResult with
        | Ok amount, Ok currency -> Ok { Amount = amount; Currency = currency }
        | Ok _, Error currencyError -> Error $"Currency error: %s{currencyError}"
        | Error amountError, Ok _ -> Error $"Amount error: %s{amountError}"
        | Error amountError, Error currencyError -> Error $"Amount error: %s{amountError} | Currency error: %s{currencyError}"
    | _ -> Error "Could not extract money"