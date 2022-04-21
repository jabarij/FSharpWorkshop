namespace FSharpWorkshop.Implementation.Finances

type Currency =
    | PLN
    | USD
    | EUR
    | GBP
    | OtherCurrency of string