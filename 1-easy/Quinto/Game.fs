module Quinto.Game

let private distribute item list =
    let rec loop pre post =
        seq {
            match post with
            | [] ->
                yield (list @ [item])
            | h::t ->
                yield (List.rev pre @ [item] @ post)
                yield! loop (h::pre) t
        }
    loop [] list

let rec private permutate = function
    | [] -> Seq.singleton []
    | h::t -> Seq.collect (distribute h) (permutate t)

let rec private candidates digits : Candidate list =
    [
        for permutation in permutate digits do
        for ops in Op.groupOf (digits.Length - 1) do
            match permutation with
            | head::ds ->
                let tail = List.zip ds ops
                match Candidate.tryCreate head tail with
                | Some candidate -> yield candidate
                | None -> ()
            | [] -> invalidArg (nameof digits) "please specify at least one digit"
    ]

let resolve digits target =
    candidates digits
    |> List.filter (fun x -> x.Result = target)
    |> List.distinctBy Candidate.normalize
    |> List.map (fun x -> x.Formula)
    |> List.sort

let verify digits target formula =
    resolve digits target
    |> List.contains formula
