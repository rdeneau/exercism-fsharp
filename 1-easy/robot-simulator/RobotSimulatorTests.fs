// This file was auto-generated based on version 3.2.0 of the canonical data.

module RobotSimulatorTests

open FsUnit.Xunit
open Xunit

open RobotSimulator

[<Fact>]
let ``At origin facing north`` () =
    let expected = create Orientation.North (0, 0)
    create Orientation.North (0, 0) |> should equal expected

[<Fact>]
let ``At negative position facing south`` () =
    let expected = create Orientation.South (-1, -1)
    create Orientation.South (-1, -1) |> should equal expected

[<Fact>]
let ``Changes north to east`` () =
    let robot = create Orientation.North (0, 0)
    let expected = create Orientation.East (0, 0)
    move "R" robot |> should equal expected

[<Fact>]
let ``Changes east to south`` () =
    let robot = create Orientation.East (0, 0)
    let expected = create Orientation.South (0, 0)
    move "R" robot |> should equal expected

[<Fact>]
let ``Changes south to west`` () =
    let robot = create Orientation.South (0, 0)
    let expected = create Orientation.West (0, 0)
    move "R" robot |> should equal expected

[<Fact>]
let ``Changes west to north`` () =
    let robot = create Orientation.West (0, 0)
    let expected = create Orientation.North (0, 0)
    move "R" robot |> should equal expected

[<Fact>]
let ``Changes north to west`` () =
    let robot = create Orientation.North (0, 0)
    let expected = create Orientation.West (0, 0)
    move "L" robot |> should equal expected

[<Fact>]
let ``Changes west to south`` () =
    let robot = create Orientation.West (0, 0)
    let expected = create Orientation.South (0, 0)
    move "L" robot |> should equal expected

[<Fact>]
let ``Changes south to east`` () =
    let robot = create Orientation.South (0, 0)
    let expected = create Orientation.East (0, 0)
    move "L" robot |> should equal expected

[<Fact>]
let ``Changes east to north`` () =
    let robot = create Orientation.East (0, 0)
    let expected = create Orientation.North (0, 0)
    move "L" robot |> should equal expected

[<Fact>]
let ``Facing north increments Y`` () =
    let robot = create Orientation.North (0, 0)
    let expected = create Orientation.North (0, 1)
    move "A" robot |> should equal expected

[<Fact>]
let ``Facing south decrements Y`` () =
    let robot = create Orientation.South (0, 0)
    let expected = create Orientation.South (0, -1)
    move "A" robot |> should equal expected

[<Fact>]
let ``Facing east increments X`` () =
    let robot = create Orientation.East (0, 0)
    let expected = create Orientation.East (1, 0)
    move "A" robot |> should equal expected

[<Fact>]
let ``Facing west decrements X`` () =
    let robot = create Orientation.West (0, 0)
    let expected = create Orientation.West (-1, 0)
    move "A" robot |> should equal expected

[<Fact>]
let ``Moving east and north from README`` () =
    let robot = create Orientation.North (7, 3)
    let expected = create Orientation.West (9, 4)
    move "RAALAL" robot |> should equal expected

[<Fact>]
let ``Moving west and north`` () =
    let robot = create Orientation.North (0, 0)
    let expected = create Orientation.West (-4, 1)
    move "LAAARALA" robot |> should equal expected

[<Fact>]
let ``Moving west and south`` () =
    let robot = create Orientation.East (2, -7)
    let expected = create Orientation.South (-3, -8)
    move "RRAAAAALA" robot |> should equal expected

[<Fact>]
let ``Moving east and north`` () =
    let robot = create Orientation.South (8, 4)
    let expected = create Orientation.North (11, 5)
    move "LAAARRRALLLL" robot |> should equal expected

