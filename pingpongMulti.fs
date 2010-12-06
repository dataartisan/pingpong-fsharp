type Message = Finished | Msg of int * int * Message MailboxProcessor

let (<--) (m:MailboxProcessor<_>) x = m.Post(x)

let sync() =
    MailboxProcessor.Start(fun inbox -> 
        let rec loop () = async {
            let! msg = inbox.Receive()
            msg()
            return! loop()
        }
        loop())

let ping id iters (sync : (unit -> unit) MailboxProcessor)
    (outbox : Message MailboxProcessor) =
    MailboxProcessor.Start(fun inbox -> 
        let rec loop n = async { 
            match n with
            | 0 -> sync <-- (fun () -> printfn "ping %d finished" id)
                   return ()
            | _ -> outbox <-- Msg(id, n, inbox)
                   let! msg = inbox.Receive()
                   sync <-- (fun () -> printfn "ping %d received pong" id)
                   return! loop(n-1)}
        loop iters)
            
let pong (sync : (unit -> unit) MailboxProcessor) =
    MailboxProcessor.Start(fun inbox -> 
        let rec loop () = async { 
            let! msg = inbox.Receive()
            match msg with
            | Finished -> 
                sync <-- (fun () -> printfn "pong finished")
                return ()
            | Msg(id, n, outbox) -> 
                sync <-- (fun () -> printfn "pong received ping %d from %d" n id)
                outbox <-- Msg(id, n, inbox)
                return! loop() }
                    
        loop())

let syncer = sync()
let ponger = pong syncer
ping 1 100 syncer <| ponger |> ignore
ping 2 100 syncer <| ponger |> ignore
ping 3 100 syncer <| ponger |> ignore
System.Console.ReadLine() |> ignore
ponger <-- Finished
//System.Threading.Thread.Sleep 100
