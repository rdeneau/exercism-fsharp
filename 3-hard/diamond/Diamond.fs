namespace Diamond

module String =
    let rev (s: string) : string =
        s |> Seq.rev |> Seq.toArray |> System.String

module Core =
    let private quadrantRowOf (length: int) (index: int) (letter: Letter) : string =
        let leftSpaces  = " " |> String.replicate index
        let rightSpaces = " " |> String.replicate (length - index - 1)
        $"{leftSpaces}{letter}{rightSpaces}"

    let private secondQuadrantOf (letter: Letter) : string list =
        let letters = Letter.upTo letter
        letters
        |> List.mapi (quadrantRowOf letters.Length)

    let private withLeftSymmetric (rows: string list) =
        rows |> List.map (fun right ->
            let left = right.Substring(1) |> String.rev
            $"{left}{right}")

    let private withBottomSymmetric (rows: string list) =
        rows @ (rows |> List.rev |> List.tail)

    let diamondOf (letter: Letter) : string list =
        secondQuadrantOf letter
        |> withLeftSymmetric
        |> withBottomSymmetric
