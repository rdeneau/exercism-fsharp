module BinarySearchTree

type Tree = { Data: int; Left: Tree option; Right: Tree option }
with
    member this.Fold seed folder =
        // Continuation Passing Style recursion
        let rec loop someTree continuation =
            match someTree with
            | None -> seed |> continuation
            | Some subtree ->
                loop subtree.Left (fun leftItems ->
                    loop subtree.Right (fun rightItems ->
                        (folder leftItems subtree rightItems) |> continuation
                    )
                )
        loop (Some this) id

    override this.ToString() =
        this.Fold "" (fun leftText subtree rightText ->
            let leftSeparator  = if leftText  <> "" then " / " else ""
            let rightSeparator = if rightText <> "" then " \ " else ""
            $"{leftText}{leftSeparator}{subtree.Data}{rightSeparator}{rightText}"
        )

let leaf data = { Data = data; Left = None; Right = None }

let left { Left = x } = x

let right { Right = x } = x

let data { Data = x } = x

let rec add item tree =
    let addItem = function
        | None -> leaf item
        | Some subtree -> subtree |> add item

    if item <= data tree then
        { tree with Left = tree |> left |> addItem |> Some }
    else
        { tree with Right = tree |> right |> addItem |> Some }

let create items =
    let rec loop tree remainingItems =
        match remainingItems with
        | [] -> tree
        | x :: xs -> loop (tree |> add x) xs

    match items with
    | [] -> invalidArg (nameof items) "Cannot be empty"
    | x :: xs -> loop (leaf x) xs

// ⚠ No tail recursion - See `sortedData`
let rec sortedDataKo tree =
    let itemsOf = function
        | None -> []
        | Some subtree -> sortedDataKo subtree

    (itemsOf tree.Left) @ [tree.Data] @ (itemsOf tree.Right)

let sortedData (tree: Tree) =
    tree.Fold [] (fun leftItems subtree rightItems ->
        leftItems @ [subtree.Data] @ rightItems)
