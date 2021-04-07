module ArmstrongNumbers

let powerOf n x = pown x n

let isArmstrongNumber (number: int): bool =
    let digits = number |> string

    let result =
        digits
        |> Seq.map (string >> int)
        |> Seq.map (powerOf digits.Length)
        |> Seq.sum

    number = result
