module List

let sequence list =
    match list |> List.isEmpty ||
          list |> List.contains None with
    | true  -> None
    | false -> Some (list |> List.map Option.get)
