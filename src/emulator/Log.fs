module Log

open System.Diagnostics

type Message = Log of string | Subscribe of (string -> unit)

let logger = MailboxProcessor.Start(fun mailbox ->
        
        let stopwatch = new Stopwatch ()
        stopwatch.Start ()

        let rec loop subscribers = async {
            
            let! msg = mailbox.Receive ()

            match msg with
            | Subscribe subscriber ->
                return! loop <| subscriber :: subscribers
            | Log message ->
                
                let message = sprintf "%d: %s" stopwatch.ElapsedMilliseconds message

                do subscribers |> List.iter (fun subscriber -> do subscriber message)
                return! loop subscribers
        }

        loop []
    )

let log = Log >> logger.Post

let logf format = Printf.kprintf log format

let subscribe = Subscribe >> logger.Post
