module RobotName

open System

type Robot = Robot of string

let name (Robot x) = x

let private random = Random(Guid.NewGuid().GetHashCode());

let private randomRobotName () =
    let letter () = ['A'..'Z'].[random.Next(0, 25)]
    let digit () = random.Next(0, 9)
    $"{letter()}{letter()}{digit()}{digit()}{digit()}"

let mutable names: string Set = Set.empty

let (|Available|Taken|) name =
    if names |> Set.contains name
    then Taken
    else Available name

let rec private uniqueRobotName () =
    match randomRobotName () with
    | Available name -> name
    | Taken -> uniqueRobotName()

let private mkRobotWith adjustNames =
    let name = uniqueRobotName()
    names <-
        names
        |> adjustNames
        |> Set.add name
    Robot name

let mkRobot () = mkRobotWith id

let reset (Robot previousName) = mkRobotWith (Set.remove previousName)
