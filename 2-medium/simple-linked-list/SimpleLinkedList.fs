module SimpleLinkedList

type LinkedList<'a> =
    | Nil
    | Item of 'a * LinkedList<'a>

let nil = Nil

let create item list = Item (item, list)

let isNil = function
    | Nil -> true
    | Item _ -> false

let next = function
    | Item (_, x) -> x
    | Nil -> failwith "Nil has no tail"

let datum = function
    | Item (x, _) -> x
    | Nil -> failwith "Nil contains no datum"

let toList linkedList =
    let rec loop result rest =
        match rest with
        | Nil -> result
        | Item (h, t) -> loop (h :: result) t
    loop [] linkedList
    |> List.rev

let fromList xs =
    let rec loop result rest =
        match rest with
        | [] -> result
        | h :: t -> loop (create h result) t
    xs
    |> List.rev
    |> loop Nil

let reverse linkedList =
    let rec loop result rest =
        match rest with
        | Nil -> result
        | Item (h, t) -> loop (create h result) t
    loop Nil linkedList
