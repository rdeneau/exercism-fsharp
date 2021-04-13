module SecretHandshake

open System

[<Flags>]
type private Command =
    | Wink         = 0b00001
    | DoubleBlink  = 0b00010
    | CloseEyes    = 0b00100
    | Jump         = 0b01000
    | ReverseOrder = 0b10000

let private execute list command =
    match command with
    | Command.Wink         -> list @ ["wink"]
    | Command.DoubleBlink  -> list @ ["double blink"]
    | Command.CloseEyes    -> list @ ["close your eyes"]
    | Command.Jump         -> list @ ["jump"]
    | Command.ReverseOrder -> list |> List.rev
    | _ -> list

let commands number =
    let command = enum<Command> number
    Enum.GetValues(typeof<Command>)
    |> Seq.cast<Command>
    |> Seq.filter command.HasFlag
    |> Seq.fold execute []
