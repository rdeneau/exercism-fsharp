// This is the file you need to modify for your own solution.
// The unit tests will use this code, and it will be used by the benchmark tests
// for the "Mine" row of the summary table.

// Remember to not only run the unit tests for this exercise, but also the
// benchmark tests using `dotnet run -c Release`:
// * Summary *
// |   Method |     Mean | Allocated |
// |--------- |---------:|----------:|
// | Baseline | 6.497 us |  13.75 KB |
// |     Mine | 5.657 us |  10.82 KB |

module TreeBuilding

open TreeBuildingTypes

type Tree = Tree of recordId: int * children: Tree list

let recordId (Tree (recordId, _)) = recordId
let children (Tree (_, children)) = children
let isBranch tree = tree |> children |> (not << List.isEmpty)

let inline tee action x = action x; x

let validate sortedRecords =
    sortedRecords
    |> Seq.pairwise
    |> Seq.iter (fun (current, next) ->
        if current.RecordId <> 0 && current.RecordId <= current.ParentId then
            failwith "Nodes with invalid parents"
        if current.RecordId <> (next.RecordId - 1) then
            failwith "Non-continuous list" )

let normalize records =
    records
    |> List.sortBy (fun x -> x.RecordId)
    |> tee validate

let buildTree records =
    match normalize records with
    | [] -> failwith "Empty input"
    | root :: _ when root.RecordId <> 0 -> failwith "Root node is invalid"
    | root :: _ when root.ParentId <> 0 -> failwith "Root node is invalid"
    | root :: others ->
        let recordsByParentId =
            others
            |> List.sortBy (fun x -> x.RecordId)
            |> List.groupBy (fun x -> x.ParentId)
            |> Map.ofList

        let rec tree record =
            let children =
                match recordsByParentId |> Map.tryFind record.RecordId with
                | Some xs -> xs |> List.map tree
                | None -> []
            Tree (record.RecordId, children)
        tree root
