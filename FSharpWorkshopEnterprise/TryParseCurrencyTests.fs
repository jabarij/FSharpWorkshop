namespace FSharpWorkshopEnterprise

open FsUnit.Xunit
open NHamcrest
open NHamcrest.Core
open Xunit
open FSharpWorkshopEnterprise.Implementation

module TryParseCurrencyTests =

    let assertIsOk (actual: Result<'ok, 'error>): 'ok =
        match actual with
        | Ok value -> value
        | Error error ->
            Assert.Fail $"Given value was Error(%s{error.ToString()})"
            failwith "not expected point of code"
            
    let assertIsError (actual: Result<'ok, 'error>): 'error =
        match actual with
        | Ok value ->
            Assert.Fail $"Given value was Ok(%s{value.ToString()})"
            failwith "not expected point of code"
        | Error error -> error
    
    [<Fact>]
    let ``Not valid currency code - should return Error with proper message`` () =
        let str = "!@#$%^"
        let actual = tryParseCurrency str
        assertIsError actual |> ignore
    
    [<Fact>]
    let ``Valid FIAT currency - should return Ok with proper value`` () =
        let str = "PLN"
        let expected = Fiat PLN
        let actual = tryParseCurrency str
        let okActual = assertIsOk actual
        Assert.Equal(expected, okActual)
    
    [<Fact>]
    let ``Valid crypto currency - should return Ok with proper value`` () =
        let str = "BTC"
        let expected = Crypto BTC
        let actual = tryParseCurrency str
        let okActual = assertIsOk actual
        Assert.Equal(expected, okActual)
    
    [<Fact>]
    let ``Valid but unknown currency - should return Ok with proper value`` () =
        let str = "CZK"
        let expected = UnknownCurrency "CZK"
        let actual = tryParseCurrency str
        let okActual = assertIsOk actual
        Assert.Equal(expected, okActual)