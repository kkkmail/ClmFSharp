namespace ClmSys

open ClmSys.Logging
open System.Threading

module TimerEvents =

    [<Literal>]
    let RefreshInterval = 30_000


    type EventHandlerInfo =
        {
            eventHandler : unit -> unit
            refreshInterfal : int option
            logger : Logger
        }

        static member defaultValue h =
            {
                eventHandler = h
                refreshInterfal = None
                logger = logger
            }


    type EventHandler(i : EventHandlerInfo) =
        let refreshInterfal =
            match i.refreshInterfal with
            | Some r -> r
            | None -> RefreshInterval
            |> float

        let mutable counter = -1

        let eventHandler _ =
            try
                if Interlocked.Increment(&counter) = 0
                then
                    try
                        i.eventHandler()
                    with
                        | e -> i.logger.logExn "Error occurred." e
                else ignore()
            finally
                Interlocked.Decrement(&counter) |> ignore

        let timer = new System.Timers.Timer(refreshInterfal)
        do timer.AutoReset <- true
        do timer.Elapsed.Add eventHandler
        //do timer.Start()

        member __.start() = do timer.Start()
