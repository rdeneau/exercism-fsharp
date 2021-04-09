module QueenAttack

// Row
type Row = Row of int

module Row =
  let parse i = if i >= 0 && i < 8 then Some (Row i) else None

// Column
type Column = Column of int

module Column =
  let parse i = if i >= 0 && i < 8 then Some (Column i) else None

// Piece
type Piece = Piece of Row * Column

module Piece =
  let parse (row, col) =
    match (Row.parse row, Column.parse col) with
    | Some r, Some c -> Piece (r, c) |> Some
    | _ -> None

  let (|SameRow|SameColumn|SameDiagonal|Other|) ((Piece (Row row1, Column col1)), (Piece (Row row2, Column col2))) =
    if col1 = col2 then SameColumn
    elif row1 = row2 then SameRow
    elif abs(col1 - col2) = abs(row1 - row2) then SameDiagonal
    else Other

  let canAttack (queen: Piece) (piece: Piece) =
    match queen, piece with
    | SameRow | SameColumn | SameDiagonal -> true
    | _ -> false

// Initial functions
let create (row, col) =
  match Piece.parse(row, col) with
  | Some _ -> true
  | _ -> false

let canAttack queen piece =
  match ((Piece.parse queen), (Piece.parse piece)) with
  | Some q, Some p -> Piece.canAttack q p
  | _ -> false