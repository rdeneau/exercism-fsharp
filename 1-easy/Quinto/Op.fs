namespace Quinto

type Op =
    | Add
    | Sub
    | Mul
    | Div

module Op =
    let char = function
        | Add -> '+'
        | Sub -> '-'
        | Mul -> '*'
        | Div -> '/'

    let tryEval x y = function
        | Add -> Some (x + y)
        | Sub -> Some (x - y)
        | Mul -> Some (x * y)
        | Div -> if (x % y = 0) then Some (x / y) else None

    let isCommutative = function
        | Add | Mul -> true
        | Sub | Div -> false

    let private allOps = [ Add; Sub; Mul; Div ]

    let private extendGroup group =
        List.allPairs allOps group
        |> List.map (fun (x, xs) -> x :: xs )

    let private groupOf1 =
        allOps |> List.map (fun x -> [x])

    let groupOf (count: int) =
        (groupOf1, [2..count])
        ||> List.fold (fun group _ -> extendGroup group)
