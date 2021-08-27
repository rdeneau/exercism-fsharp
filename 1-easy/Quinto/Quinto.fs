module Quinto

type OpFn = int -> int -> int

type Op = char * OpFn

module Op =
    let add = '+', (+)
    let sub = '-', (-)
    let mul = '*', (*)
    let div = '/', (/)

    let isCommutative ((op, _): Op) =
        match op with
        | '+' | '*' -> true
        | _ -> false

    let all : Op[] = [| add; sub; mul; div |]

    let groupsOfFour =
        seq {
            for i1 in [0..3] do
            for i2 in [0..3] do
            for i3 in [0..3] do
            for i4 in [0..3] do
                yield (all.[i1], all.[i2], all.[i3], all.[i4]) }

type Digop = int * Op

type Candidate =
    { First: int
      Digop1: Digop
      Digop2: Digop
      Digop3: Digop
      Digop4: Digop }

module Candidate =
    let eval candidate =
        let d1, (_,op1) = candidate.Digop1
        let d2, (_,op2) = candidate.Digop2
        let d3, (_,op3) = candidate.Digop3
        let d4, (_,op4) = candidate.Digop4

        let t0 = candidate.First
        let t1 = op1 t0 d1
        let t2 = op2 t1 d2
        let t3 = op3 t2 d3
        let t4 = op4 t3 d4
        t4

    let formula candidate =
        let d1, (op1, fn1) = candidate.Digop1
        let d2, (op2, fn2) = candidate.Digop2
        let d3, (op3, fn3) = candidate.Digop3
        let d4, (op4, fn4) = candidate.Digop4

        let t0 = candidate.First
        let t1 = fn1 t0 d1
        let t2 = fn2 t1 d2
        let t3 = fn3 t2 d3
        let t4 = fn4 t3 d4

        $"{t0} {op1} {d1} = {t1} {op2} {d2} = {t2} {op3} {d3} = {t3} {op4} {d4} = {t4}"

    let normalize candidate =
        let d0 = candidate.First
        let d1, op1 = candidate.Digop1

        let d0, d1 =
            if Op.isCommutative op1
            then (min d0 d1), (max d0 d1)
            else d0, d1

        { candidate with First = d0; Digop1 = d1, op1 }

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
        for (o1,o2,o3,o4) in Op.groupsOfFour do
            match permutation with
            | [d0;d1;d2;d3;d4] ->
                yield { First  = d0
                        Digop1 = d1, o1
                        Digop2 = d2, o2
                        Digop3 = d3, o3
                        Digop4 = d4, o4 }
            | _ -> invalidArg (nameof digits) "5 items expected"
    ]

let resolve digits target =
    candidates digits
    |> List.filter (fun x -> Candidate.eval x = target)
    |> List.distinctBy (Candidate.normalize >> Candidate.formula)
    |> List.map Candidate.formula
    |> List.sort

let verify digits target formula =
    resolve digits target
    |> List.contains formula
