namespace Diamond

type Letter =
    | A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

module Letter =
    let private all =
        [
            A ; B ; C ; D ; E ; F ; G ; H ; I ; J ; K ; L ; M
            N ; O ; P ; Q ; R ; S ; T ; U ; V ; W ; X ; Y ; Z
        ]
        |> List.mapi (fun index letter -> (letter, (index, (string letter).[0])))
        |> Map.ofList

    let ofChar (c: char) : Letter =
        all |> Map.findKey (fun _ (_, char) -> char = c)

    let toChar (letter: Letter) : char =
        snd all.[letter]

    let index (letter: Letter) : int =
        fst all.[letter]

    let upTo (letter: Letter) : Letter list =
        [ 'A' .. (letter |> toChar) ]
        |> List.map ofChar
