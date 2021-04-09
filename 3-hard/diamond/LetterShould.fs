module Diamond.LetterShould

open Swensen.Unquote
open Xunit

[<Fact>]
let ``Produces letters up to the given one`` () =
    [
        Letter.upTo A
        Letter.upTo B
        Letter.upTo E
    ]
    =!
    [
        [ A ]
        [ A; B ]
        [ A; B; C; D; E ]
    ]