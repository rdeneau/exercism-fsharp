module BookStore

type Quantity = Quantity of int

module Quantities =
    let private normalize quantities =
        quantities
        |> List.sortDescending
        |> List.map Quantity

    let ofList (books: int list) =
        books
        |> List.countBy id
        |> List.map snd
        |> normalize

    let decrement size quantities =
        quantities
        |> List.mapi (fun i (Quantity qty) ->
            if (i + 1) <= size
            then qty - 1
            else qty )
        |> normalize

type Group = Group of int

module Group =
    let private BookPrice = 8.00m
    let private MaxSize = 5

    let private discount (Group size) =
        match size with
        | 1 -> 0.00m
        | 2 -> 0.05m
        | 3 -> 0.10m
        | 4 -> 0.20m
        | 5 -> 0.25m
        | n -> invalidArg (nameof size) $"Group of {n} not supported"

    let private priceOne (groupSize & Group size) =
        let factor = 1.00m - (discount groupSize)
        BookPrice * (decimal size) * factor

    let price groupSizes =
        groupSizes |> List.sumBy priceOne

    let computeAllCombinations quantities =
        let rec loop (quantities: Quantity list) (groupCombinations: Group list list) =
            let quantities = quantities |> List.filter (fun (Quantity x) -> x > 0)
            match quantities.Length with
            | 0 -> groupCombinations
            | n ->
                let maxGroupSize = min 5 n
                [ for size = maxGroupSize downto 1 do
                    let quantities' = quantities |> Quantities.decrement size
                    let groupCombinations' =
                        match groupCombinations with
                        | [] -> [[Group size]]
                        | groups :: otherGroupCombinations -> ((Group size) :: groups) :: otherGroupCombinations
                    yield! loop quantities' groupCombinations'
                ]
                |> List.map List.sortDescending
                |> List.distinct
        loop quantities []

let total (books: int list) : decimal =
    if books |> List.isEmpty then
        0.00m
    else
        books
        |> Quantities.ofList
        |> Group.computeAllCombinations
        |> List.map Group.price
        |> List.min
