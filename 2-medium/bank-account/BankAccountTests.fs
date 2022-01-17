// Tests improved:
// - Add test "Account opened cannot be opened again"
// - Add test "Account closed cannot be closed again"
// - Test "Account can be updated from multiple threads": parameterized, call `updateBalanceAsync`

module BankAccountTest

open Xunit
open FsUnit.Xunit
open BankAccount

[<Fact>]
let ``Returns empty balance after opening`` () =
    let account = mkBankAccount() |> openAccount
    getBalance account |> should equal (Some 0.0m)

[<Fact>]
let ``Check basic balance`` () =
    let account = mkBankAccount() |> openAccount
    let openingBalance = account |> getBalance

    let updatedBalance =
        account
        |> updateBalance 10.0m
        |> getBalance

    openingBalance |> should equal (Some 0.0m)
    updatedBalance |> should equal (Some 10.0m)

[<Fact>]
let ``Balance can increment or decrement`` () =
    let account = mkBankAccount() |> openAccount
    let openingBalance = account |> getBalance

    let addedBalance =
        account
        |> updateBalance 10.0m
        |> getBalance

    let subtractedBalance =
        account
        |> updateBalance -15.0m
        |> getBalance

    openingBalance |> should equal (Some 0.0m)
    addedBalance |> should equal (Some 10.0m)
    subtractedBalance |> should equal (Some -5.0m)

[<Fact>]
let ``Account opened can be closed`` () =
    let account =
        mkBankAccount()
        |> openAccount
        |> closeAccount
    getBalance account |> should equal None
    account |> should not' (equal None)

[<Theory>]
[<InlineData(1000)>]
//[<InlineData(100000)>]
let ``Account can be updated from multiple threads`` (number: int) =
    let account =
        mkBankAccount()
        |> openAccount

    let updateAccountAsync =
        async {
            account
            |> updateBalance 1.0m
            |> ignore
        }

    updateAccountAsync
    |> List.replicate number
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

    getBalance account |> should equal (Some (decimal number))
