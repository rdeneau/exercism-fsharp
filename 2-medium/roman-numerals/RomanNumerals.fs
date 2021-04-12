module RomanNumerals

let private romanDigit (one, five, ten) arabicDigit =
    match arabicDigit with
    | 0 -> ""
    | 1 -> one
    | 2 -> one + one
    | 3 -> one + one + one
    | 4 -> one + five
    | 5 -> five
    | 6 -> five + one
    | 7 -> five + one + one
    | 8 -> five + one + one + one
    | 9 -> one + ten
    | _ -> invalidArg (nameof arabicDigit) $"expecting one digit (0-9), received {arabicDigit}"

// Split number into digits ordered by power of 10
// E.g. 123 = 3 + 20 + 300 -> [ 3 2 1 ]
let digitsOf number =
    number
    |> string
    |> Seq.map (string >> int)
    |> Seq.rev
    |> Seq.toList

let roman arabicNumeral =
    let digits = digitsOf arabicNumeral
    let lettersByDigit =
        [ ("I", "V", "X")
          ("X", "L", "C")
          ("C", "D", "M")
          ("M", "MMM", "?") ]
        |> List.truncate digits.Length

    List.zip digits lettersByDigit
    |> List.map (fun (digit, letters) -> romanDigit letters digit)
    |> List.rev
    |> String.concat ""
