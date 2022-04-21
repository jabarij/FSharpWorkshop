namespace FSharpWorkshopEnterprise

open FsUnit.Xunit
open NHamcrest
open NHamcrest.Core
open Xunit
open FSharpWorkshopEnterprise.Implementation

module TryParseMoneyTests =

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
    let ``Not valid amount decimal - should return Error`` () =
        let str = "abcd USD"
        let actual = tryParseMoney str
        assertIsError actual |> ignore
    
    [<Fact>]
    let ``Valid amount of FIAT currency - should return Ok with proper value`` () =
        let str = "123,456 PLN"
        let expected = { Amount = 123.456M; Currency = Fiat PLN }
        let actual = tryParseMoney str
        let okActual = assertIsOk actual
        Assert.Equal(expected, okActual)
    
    [<Fact>]
    let ``Valid amount of crypto currency - should return Ok with proper value`` () =
        let str = "123,456 BTC"
        let expected = { Amount = 123.456M; Currency = Crypto BTC }
        let actual = tryParseMoney str
        let okActual = assertIsOk actual
        Assert.Equal(expected, okActual)
    
    [<Fact>]
    let ``Valid amount of valid unknown currency - should return Ok with proper value`` () =
        let str = "123,456 CZK"
        let expected = { Amount = 123.456M; Currency = UnknownCurrency "CZK" }
        let actual = tryParseMoney str
        let okActual = assertIsOk actual
        Assert.Equal(expected, okActual)