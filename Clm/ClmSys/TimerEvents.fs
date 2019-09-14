namespace ClmSys

open ClmSys.Logging
open System.Threading
open System

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
        let handlerId = Guid.NewGuid()

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
                        //printfn "EventHandler: %A starting..." handlerId
                        i.eventHandler()
                    with
                        | e -> i.logger.logExn "Error occurred." e
                else
                    printfn "!!! ERROR - EventHandler: %A is still running !!!" handlerId
                    ignore()
            finally
                Interlocked.Decrement(&counter) |> ignore
                //printfn " ...EventHandler: %A completed." handlerId

        let timer = new System.Timers.Timer(refreshInterfal)
        do timer.AutoReset <- true
        do timer.Elapsed.Add eventHandler

        member __.start() = do timer.Start()
