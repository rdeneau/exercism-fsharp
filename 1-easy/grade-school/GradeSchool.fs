module GradeSchool

type School = Map<int, string list>

let empty: School = Map.empty

let add (student: string) (grade: int) (school: School): School =
    school |> Map.change grade
        ( function
          | Some xs -> Some (student :: xs |> List.sort)
          | None    -> Some [student] )

let roster (school: School): string list =
    school
    |> Map.toList
    |> List.sortBy fst
    |> List.collect snd

let grade (number: int) (school: School): string list =
    school
    |> Map.tryFind number
    |> Option.defaultValue []
