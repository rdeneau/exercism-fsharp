module ErrorHandling

open System

let handleErrorByThrowingException() =
    failwith "Simulate exception"

let handleErrorByReturningOption (input: string) =
    match Int32.TryParse input with
    | true, n -> Some n
    | false, _ -> None

let handleErrorByReturningResult (input: string) =
    match Int32.TryParse input with
    | true, n -> Ok n
    | false, _ -> Error "Could not convert input to integer"

let bind switchFunction twoTrackInput =
    // 💡 Built-in: twoTrackInput |> Result.bind switchFunction
    match twoTrackInput with
    | Ok x -> switchFunction x
    | Error e -> Error e

let cleanupDisposablesWhenThrowingException resource =
    use r = resource
    failwith "Simulate exception"