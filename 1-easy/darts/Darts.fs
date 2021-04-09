module Darts

let distanceToOrigin point =
    point
    |> List.sumBy (fun x -> x ** 2.0)
    |> sqrt

let (|Inner|Middle|Outer|Outside|) point =
    let radiusOfCircle = {| Outer = 10.0 ; Middle = 5.0 ; Inner = 1.0 |}

    match distanceToOrigin point with
    | x when x > radiusOfCircle.Outer  -> Outside
    | x when x > radiusOfCircle.Middle -> Outer
    | x when x > radiusOfCircle.Inner  -> Middle
    | _                                -> Inner

let score (x: double) (y: double): int =
    match [ x; y ] with
    | Outside -> 0
    | Outer   -> 1
    | Middle  -> 5
    | Inner   -> 10
