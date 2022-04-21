namespace FSharpWorkshop.Implementation.Finances

type Balance (values: Map<Currency, decimal>) =
    member private _._values = values
    member private _._update (value: Money) fChange =
        values
        |> Map.change value.Currency fChange
        |> Balance
    member this.add (value: Money) =
        this._update value (fun current ->
            current
            |> Option.map (value.Amount |> (+))
            |> Option.orElseWith (fun () -> Some value.Amount))
    static member empty =
        []
        |> Map.ofList
        |> Balance
    static member toList (balance: Balance): Money list =
        balance._values
        |> Map.toList
        |> List.map (fun (currency, amount) -> { Amount = amount; Currency = currency })