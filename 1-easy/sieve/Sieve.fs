module Sieve

let primes limit =
    let maxMultiplier =
        limit |> float |> sqrt |> floor |> int
    let candidates =
        if limit < 2
        then []
        else 2 :: [3 .. 2 .. limit]
    (candidates, candidates)
    ||> List.fold (fun primes candidate ->
        let multiples = [ for multiplier in 2 .. maxMultiplier do candidate * multiplier ]
        primes |> List.except multiples )
