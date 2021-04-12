module Matrix

let row num (matrix: string) =
    let rows =
        matrix.Split('\n')
        |> List.ofArray
        |> List.map (fun s -> s.Split(' ') |> List.ofArray |> List.map int)
    rows.[num-1]

let column index matrix = failwith "You need to implement this function."
