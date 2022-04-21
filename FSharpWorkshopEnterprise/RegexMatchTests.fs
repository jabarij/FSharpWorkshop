namespace FSharpWorkshopEnterprise

open FsUnit.Xunit
open NHamcrest
open NHamcrest.Core
open Xunit
open FSharpWorkshopEnterprise.Implementation

module RegexMatchTests =

    let assertEqualMaps (expected: Map<'key, 'value>) (actual: Map<'key, 'value>): unit =
        let expectedEn = System.Linq.Enumerable.AsEnumerable(expected)
        let actualEn = System.Linq.Enumerable.AsEnumerable(actual)
        Assert.Equal<System.Collections.Generic.KeyValuePair<'key, 'value>>(expectedEn, actualEn) |> ignore
        ()
        
    let assertIsSome (actual: 'a option): 'a =
        if actual.IsNone then
            Assert.Fail("Given value was None")
        actual.Value
    
    [<Fact>]
    let ``Input does not match the regex pattern - should return None`` () =
        let pattern = "[a-z]"
        let input = "123"
        let expected = None
        let actual =
            match input with
            | RegexMatch pattern regexResult ->  Some regexResult
            | _ -> None
        actual |> should equal expected
        
    [<Fact>]
    let ``Input matches the regex pattern - should return one indexed group`` () =
        let pattern = "[0-9]"
        let input = "a2b"
        let expected =
            [ "0", "2" ]
            |> Map.ofList
        let actual =
            match input with
            | RegexMatch pattern regexResult ->  Some regexResult
            | _ -> None
        let someActual = assertIsSome actual
        assertEqualMaps expected someActual
        
    [<Fact>]
    let ``Input matches the regex pattern - should return one named group`` () =
        let pattern = "(?<num>[0-9])"
        let input = "a2b"
        let expected =
            [ "0", "2"
              "num", "2" ]
            |> Map.ofList
        let actual =
            match input with
            | RegexMatch pattern regexResult ->  Some regexResult
            | _ -> None
        let someActual = assertIsSome actual
        assertEqualMaps expected someActual
        
    [<Fact>]
    let ``Input matches the regex pattern with one of two groups - should return only first group`` () =
        let pattern = "(?<matched>[0-4])|(?<not_matched>[5-9])"
        let input = "1b"
        let expected =
            [ "0", "1"
              "matched", "1" ]
            |> Map.ofList
        let actual =
            match input with
            | RegexMatch pattern regexResult ->  Some regexResult
            | _ -> None
        let someActual = assertIsSome actual
        assertEqualMaps expected someActual