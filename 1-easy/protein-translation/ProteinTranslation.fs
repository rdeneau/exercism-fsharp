module ProteinTranslation

type Codon = Protein of string | Stop

let private tryParseCodon codon =
    match codon with
    | "AUG"                         -> Some (Protein "Methionine")
    | "UUU" | "UUC"                 -> Some (Protein "Phenylalanine")
    | "UUA" | "UUG"                 -> Some (Protein "Leucine")
    | "UCU" | "UCC" | "UCA" | "UCG" -> Some (Protein "Serine")
    | "UAU" | "UAC"                 -> Some (Protein "Tyrosine")
    | "UGU" | "UGC"                 -> Some (Protein "Cysteine")
    | "UGG"                         -> Some (Protein "Tryptophan")
    | "UAA" | "UAG" | "UGA"         -> Some Stop
    | _                             -> None

let proteins rna =
    let codons =
        [ 0 .. 3 .. (String.length rna) - 1 ]
        |> List.map (fun i -> rna.Substring(i, 3))
        |> List.choose tryParseCodon

    let proteinsUntilStop (stopped, results) current =
        match (stopped, current) with
        | (true, _)
        | (false, Stop) -> (true, results)
        | (false, Protein p) -> (false, p :: results) 

    codons
    |> List.fold proteinsUntilStop (false, [])
    |> snd
    |> List.rev
