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

let rec private candidates digits =
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

let private performResolve predicate digits target =
    candidates digits
    |> List.filter (fun x -> predicate x && Candidate.hasResult target x)
    |> List.distinctBy Candidate.normalize
    |> List.map Candidate.formula
    |> List.sort

let resolve digits target =
    performResolve (fun _ -> true) digits target

let resolveStrictly digits target =
    performResolve (not << Candidate.containsIdentity) digits target
