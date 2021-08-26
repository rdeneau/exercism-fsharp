module QuintoTests

open Swensen.Unquote
open Xunit

[<Fact>]
let ``3 * 5 = 15 + 4 = 19 * 2 = 38 - 1 = 37`` () =
    (Quinto.resolve [1;2;3;4;5] 37) =! "3 * 5 = 15 + 4 = 19 * 2 = 38 - 1 = 37"
