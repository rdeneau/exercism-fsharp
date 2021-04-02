module RobotSimulator

type Direction = North | East | South | West
type Move      = int * int
type Position  = int * int
type Robot     = { direction: Direction; position: Position }

let create direction position =
    { direction = direction; position = position }

let turn next robot =
    { robot with direction = next robot.direction }

let private advance (moveTowards: Direction -> Move) robot =
    let x,y = robot.position
    let dx,dy = moveTowards robot.direction
    { robot with position = (x+dx, y+dy) }

let private right direction =
    match direction with
    | North -> East
    | East  -> South
    | South -> West
    | West  -> North

let private left direction =
    match direction with
    | North -> West
    | West  -> South
    | South -> East
    | East  -> North

let private once direction =
    match direction with
    | North ->  0, +1
    | South ->  0, -1
    | West  -> -1,  0
    | East  -> +1,  0

let private instruct robot instruction =
    robot
    |> match instruction with
       | 'R' -> turn right
       | 'L' -> turn left
       | 'A' -> advance once
       | _ -> invalidArg (nameof instruction) $"Instruction invalid: {instruction}"

let move instructions robot : Robot =
    instructions
    |> Seq.fold instruct robot
