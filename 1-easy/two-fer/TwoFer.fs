module TwoFer

let twoFer (input: string option): string =
  let name = defaultArg input "you"
  $"One for {name}, one for me."
