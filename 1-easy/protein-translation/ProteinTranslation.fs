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

let private isProtein codon =
    match codon with
    | Protein _ -> true
    | _ -> false

let private justProtein codon =
    match codon with
    | Protein p -> Some p
    | _ -> None

let [<Literal>] CodonSize = 3

let proteins rna =
    let codons =
        rna
        |> Seq.chunkBySize CodonSize
        |> Seq.map System.String
        |> Seq.choose tryParseCodon

    codons
    |> Seq.takeWhile isProtein
    |> Seq.choose justProtein
    |> Seq.toList
