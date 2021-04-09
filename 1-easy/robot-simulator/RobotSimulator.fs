module RobotSimulator

type Orientation = North | East | South | West
type Move     = int * int
type Position = int * int
type Robot = { orientation: Orientation; position: Position }

let create orientation position =
    { orientation = orientation; position = position }

let turn next robot =
    { robot with orientation = next robot.orientation }

let private advance (moveTowards: Orientation -> Move) robot =
    let x,y = robot.position
    let dx,dy = moveTowards robot.orientation
    { robot with position = (x+dx, y+dy) }

let private right orientation =
    match orientation with
    | North -> East
    | East  -> South
    | South -> West
    | West  -> North

let private left orientation =
    match orientation with
    | North -> West
    | West  -> South
    | South -> East
    | East  -> North

let private once orientation =
    match orientation with
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
