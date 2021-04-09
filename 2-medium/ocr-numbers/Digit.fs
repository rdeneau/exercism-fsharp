namespace OcrDigits

type Digit =
    | Digit of char
    | Garbled

module Digit =
    let private digits =
        [
            [ " _ "
              "| |"
              "|_|"
              "   " ], Digit '0'
            [ "   "
              "  |"
              "  |"
              "   " ], Digit '1'
            [ " _ "
              " _|"
              "|_ "
              "   " ], Digit '2'
            [ " _ "
              " _|"
              " _|"
              "   " ], Digit '3'
            [ "   "
              "|_|"
              "  |"
              "   " ], Digit '4'
            [ " _ "
              "|_ "
              " _|"
              "   " ], Digit '5'
            [ " _ "
              "|_ "
              "|_|"
              "   " ], Digit '6'
            [ " _ "
              "  |"
              "  |"
              "   " ], Digit '7'
            [ " _ "
              "|_|"
              "|_|"
              "   " ], Digit '8'
            [ " _ "
              "|_|"
              " _|"
              "   " ], Digit '9'
        ]
        |> List.map (fun (rows, digit) ->
            (rows |> Grid.tryParseGrid |> Option.get, digit))
        |> Map.ofList

    let recognize grid =
        digits
        |> Map.tryFind grid
        |> Option.defaultValue Garbled

    let char digit =
        match digit with
        | Garbled -> '?'
        | Digit c -> c
