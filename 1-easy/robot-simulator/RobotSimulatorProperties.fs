module RobotSimulatorProperties

open FsCheck
open FsCheck.Xunit
open RobotSimulator

type Direction = L | R
type Instruction = A | L | R

[<Property>]
let ``Keep original properties after turning 4 times``
    (robot: Robot) (direction: Direction) =

    let instructions = String.replicate 4 $"{direction}"
    robot = (robot |> move instructions)

[<Property>]
let ``Keep original properties after a move in square``
    (robot: Robot) (direction: Direction) (length: PositiveInt) =

    let line = String.replicate length.Get "A"
    let instructions = String.replicate 4 $"{line}{direction}"
    robot = (robot |> move instructions)

[<Property>]
let ``Does not go further than number of instructions``
    (robot: Robot) (instructionList: Instruction list) =

    let instructionCount = instructionList.Length
    let (x1, y1) = robot.position
    let xMin = x1 - instructionCount
    let xMax = x1 + instructionCount
    let yMin = y1 - instructionCount
    let yMax = y1 + instructionCount

    let instructions = instructionList |> List.map string |> String.concat ""
    let actual = robot |> move instructions
    let (x2, y2) = actual.position

    x2 >= xMin && x2 <= xMax && y2 >= yMin && y2 <= yMax
