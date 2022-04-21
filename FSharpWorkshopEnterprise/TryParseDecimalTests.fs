namespace FSharpWorkshopEnterprise

open FsUnit.Xunit
open NHamcrest
open NHamcrest.Core
open Xunit
open FSharpWorkshopEnterprise.Implementation

module TryParseDecimalTests =

    let assertIsOk (actual: Result<'ok, 'error>): 'ok =
        match actual with
        | Ok value -> value
        | Error error ->
            Assert.Fail $"Given value was Error(%s{error.ToString()})"
            failwith "not expected point of code"
            
    let assertIsError (assertError: 'error -> unit) (actual: Result<'ok, 'error>): unit =
        match actual with
        | Ok value ->
            Assert.Fail $"Given value was Ok(%s{value.ToString()})"
            failwith "not expected point of code"
        | Error error -> assertError error
    
    [<Fact>]
    let ``Not valid Slavic decimal - should return Error with proper message`` () =
        let str = "123,456"
        let expected = 123.456M
        let actual = tryParseDecimal str
        let okActual = assertIsOk actual
        Assert.Equal(expected, okActual)
    
    [<Fact>]
    let ``Valid Slavic decimal - should return Ok with proper value`` () =
        let str = "123,456"
        let expected = 123.456M
        let actual = tryParseDecimal str
        let okActual = assertIsOk actual
        Assert.Equal(expected, okActual)