module BankAccount

[<AutoOpen>]
module Domain =
    type Transaction = Transaction of decimal

    type Balance = Balance of decimal with
        static member value(Balance balance) = balance
        static member sumOf(transactions: Transaction list) =
            transactions
            |> List.sumBy (fun (Transaction x) -> x)
            |> Balance

    type Message =
        | Open
        | Add of Transaction
        | GetBalance of AsyncReplyChannel<Balance option>
        | Close

    type Account() =
        let agent = MailboxProcessor<Message>.Start(fun inbox ->
            let rec closed () =
                async {
                    match! inbox.Receive() with
                    | Open -> return! opened [Transaction 0.0m]
                    | Close | Add _ -> return! closed () // No op
                    | GetBalance channel ->
                        channel.Reply None
                        return! closed ()
                }
            and opened (transactions: Transaction list) =
                async {
                    match! inbox.Receive() with
                    | Open -> return! opened transactions // No op
                    | Close -> return! closed ()
                    | Add transaction -> return! opened (transaction :: transactions)
                    | GetBalance channel ->
                        channel.Reply (Some (Balance.sumOf transactions))
                        return! opened transactions
                }
            closed () )

        member _.Balance = agent.PostAndReply(GetBalance)
        member this.Open() = agent.Post(Open); this
        member this.Close() = agent.Post(Close); this
        member this.Add(transaction: Transaction) = agent.Post(Add transaction); this

[<AutoOpen>]
module API =
    let mkBankAccount = Account
    let openAccount (account: Account) = account.Open()
    let closeAccount (account: Account) = account.Close()
    let getBalance (account: Account) = account.Balance |> Option.map Balance.value
    let updateBalance transaction (account: Account) = account.Add(Transaction transaction)
