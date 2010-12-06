type Message = Finished | Msg of int * int * Message MailboxProcessor

let (<--) (m:MailboxProcessor<_>) x = m.Post(x)

let ping id iters (outbox : Message MailboxProcessor) =
    MailboxProcessor.Start(fun inbox -> 
        let rec loop n = async { 
            match n with
            | 0 -> outbox <-- Finished
                   printfn "ping %d finished" id
                   return ()
            | _ -> outbox <-- Msg(id, n, inbox)
                   let! msg = inbox.Receive()
                   printfn "ping %d received pong" id
                   return! loop(n-1)}
        loop iters)
            
let pong() =
    MailboxProcessor.Start(fun inbox -> 
        let rec loop () = async { 
            let! msg = inbox.Receive()
            match msg with
            | Finished -> 
                printfn "pong finished"
                return ()
            | Msg(id, n, outbox) -> 
                printfn "pong received ping %d from %d" n id
                outbox <-- Msg(id, n, inbox)
                return! loop() }
                    
        loop())

let ponger = pong()
ping 1 100 <| ponger |> ignore
//ping 2 100 <| ponger |> ignore
//ping 3 100 <| ponger |> ignore
System.Console.ReadLine() |> ignore
