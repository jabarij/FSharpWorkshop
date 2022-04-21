namespace FSharpWorkshopEnterprise

open System
open System.Globalization
open System.Text.RegularExpressions
open Microsoft.FSharp.Core

module Implementation =

    type FiatCurrency =
        | PLN
        | USD
        | EUR
        | GBP
        | OtherFiatCurrency of string

    type CryptoCurrency =
        | BTC
        | ETH
        | USDC
        | OtherCryptoCurrency of string

    type Currency =
        | Fiat of FiatCurrency
        | Crypto of CryptoCurrency
        | UnknownCurrency of string
        
    type Money = {
        Amount: decimal
        Currency: Currency
    }

    let (|RegexMatch|_|) (pattern: string) (input: string): Map<string, string> option =
        let m = Regex.Match(input, pattern)
        if not m.Success
        then None
        else
            let groups =
                m.Groups
                |> Seq.cast<Group>
            let map =
                groups
                |> Seq.filter (fun group -> group.Success)
                |> Seq.map (fun group -> (group.Name, group.Value))
                |> Map.ofSeq
            Some map

    let tryParseDecimal (str: string): Result<decimal, string> =
        let numberStyle = NumberStyles.Number
        let culture = CultureInfo.GetCultureInfo("pl-PL")
        match Decimal.TryParse(str, numberStyle, culture) with
        | true, value -> Ok value
        | false, _ -> Error $"Invalid decimal: %s{str}"
        
    let tryParseKnownFiatCurrency (code: string): Result<FiatCurrency, string> =
        match code with
        | null | "" -> Error "Null or empty string"
        | "USD" -> Ok USD
        | "PLN" -> Ok PLN
        | "EUR" -> Ok EUR
        | "GBP" -> Ok GBP
        | _ -> Error $"Invalid or unknown FIAT currency code: %s{code}"
        
    let tryParseKnownCryptoCurrency (code: string): Result<CryptoCurrency, string> =
        match code with
        | null | "" -> Error "Null or empty string"
        | "BTC" -> Ok BTC
        | "ETH" -> Ok ETH
        | "USDC" -> Ok USDC
        | _ -> Error $"Invalid or unknown crypto currency code: %s{code}"
        
    let tryParseCurrency (str: string): Result<Currency, string> =
        let _optionToResult fNone = function
            | Some value -> Ok value
            | None -> Error (fNone())
        let _resultToOption = function
            | Ok value -> Some value
            | Error _ -> None
        let result =
            match str with
            | null | "" -> Error "Null or empty string"
            | RegexMatch "(?<code>[a-zA-Z]{3,4})" regexResults ->
                let currencyCode =
                    regexResults
                    |> Map.tryFind "code"
                let currency =
                    currencyCode
                    |> Option.map (fun code ->
                        let _tryFiat () =
                            tryParseKnownFiatCurrency code
                            |> _resultToOption
                            |> Option.map Fiat
                        let _tryCrypto () =
                            tryParseKnownCryptoCurrency code
                            |> _resultToOption
                            |> Option.map Crypto
                        let result =
                            _tryFiat ()
                            |> Option.orElseWith _tryCrypto
                            |> Option.defaultWith (fun () -> UnknownCurrency code)
                        Ok result)
                    |> Option.defaultWith (fun () -> Error "Could not extract currency code")
                currency
            | _ -> Error $"Unknown currency: %s{str}"
        result
    
    let tryParseMoney (str: string): Result<Money, string> =
        match str with
        | null | "" -> Error "Null or empty string"
        | RegexMatch "(?<amount>\d+(,\d+)?)\s(?<currency>[a-zA-Z]{3,4})" matches ->
            let amountResult =
                matches
                |> Map.tryFind "amount"
                |> Option.map tryParseDecimal
                |> Option.defaultWith (fun () -> Error "Could not extract amount")
            let currencyResult =
                matches
                |> Map.tryFind "currency"
                |> Option.map tryParseCurrency
                |> Option.defaultWith (fun () -> Error "Could not extract currency")
            match amountResult, currencyResult with
            | Ok amount, Ok currency -> Ok { Amount = amount; Currency = currency }
            | Ok _, Error currencyError -> Error $"Currency error: %s{currencyError}"
            | Error amountError, Ok _ -> Error $"Amount error: %s{amountError}"
            | Error amountError, Error currencyError -> Error $"Amount error: %s{amountError} | Currency error: %s{currencyError}"
        | _ -> Error "Could not extract money"