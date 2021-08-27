namespace Quinto

module Candidate =
    type Candidate = private {
        Head: int
        Tail: (int * Op) list
        Formula: string
        Result: int
    }

    let private singleton head =
        { Head = head; Tail = []; Formula = head.ToString("0"); Result = head }

    let private tryAdd (digit, op) candidate =
        op |> Op.tryEval candidate.Result digit
        |> Option.map (fun result ->
            let formula = $"{candidate.Formula} {op |> Op.char} {digit} = {result}"
            { candidate with
                Tail    = candidate.Tail @ [digit, op]
                Result  = result
                Formula = formula } )

    let tryCreate head tail =
        (Some (singleton head), tail)
        ||> List.fold (fun acc (digit, op) ->
            acc |> Option.bind (tryAdd (digit, op)) )

    let formula candidate =
        candidate.Formula

    let result candidate =
        candidate.Result

    let hasResult result candidate =
        result = candidate.Result

    let normalize candidate =
        match candidate.Tail with
        | [] -> candidate
        | (d1, op1) :: tail ->
            let d0 = candidate.Head
            let d0', d1' =
                if Op.isCommutative op1
                then (min d0 d1), (max d0 d1)
                else d0, d1
            match tryCreate d0' ((d1', op1) :: tail) with
            | Some candidate' -> candidate'
            | None -> failwith "Normalization should not invalidate"

    let containsIdentity candidate =
        candidate.Tail
        |> List.exists (fun (digit, op) -> op |> Op.hasNeutralElement digit)
