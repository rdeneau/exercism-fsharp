namespace Diamond

type Letter =
    | A | B | C | D | E | F | G | H | I | J | K | L | M
    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z

type Index = int

module Letter =
    let private all: Map<Letter, (Index * char)> =
        [
            A ; B ; C ; D ; E ; F ; G ; H ; I ; J ; K ; L ; M
            N ; O ; P ; Q ; R ; S ; T ; U ; V ; W ; X ; Y ; Z
        ]
        |> List.mapi (fun index letter -> (letter, (index, (string letter).[0])))
        |> Map.ofList

    let ofChar (char: char) : Letter =
        all |> Map.findKey (fun _ (_, c) -> c = char)

    let toChar (letter: Letter) : char =
        all.[letter] |> snd

    let index (letter: Letter) : int =
        all.[letter] |> fst

    let upTo (letter: Letter) : Letter list =
        [ 'A' .. (letter |> toChar) ]
        |> List.map ofChar
