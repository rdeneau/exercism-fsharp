module Leap

let (|DivisibleBy|_|) factor year =
  match year % factor with
  | 0 -> Some ()
  | _ -> None

let leapYear year =
  match year with
  | DivisibleBy 400 _ -> true
  | DivisibleBy 100 _ -> false
  | DivisibleBy   4 _ -> true
  | _                 -> false
