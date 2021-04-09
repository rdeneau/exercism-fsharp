module CollatzConjecture

let private (|NegativeOrZero|One|PositiveEven|PositiveOdd|) number =
    if number <= 0 then NegativeOrZero
    elif number = 1 then One
    elif number % 2 = 0 then PositiveEven
    else PositiveOdd

let steps (number: int): int option =
    let rec loop n count =
        match n with
        | NegativeOrZero -> None
        | One            -> Some count
        | PositiveEven   -> loop (n / 2) (count + 1)
        | PositiveOdd    -> loop (3 * n + 1) (count + 1)
    loop number 0