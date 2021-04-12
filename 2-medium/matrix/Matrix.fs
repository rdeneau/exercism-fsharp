module Matrix

let private rowsOf (matrix: string) =
    matrix.Split('\n')
    |> List.ofArray
    |> List.map (fun s -> s.Split(' ') |> List.ofArray |> List.map int)

let private itemAt num = List.item (num - 1)

let row num (matrix: string) =
    rowsOf matrix
    |> itemAt num

let column num (matrix: string) =
    rowsOf matrix
    |> List.map (itemAt num)
