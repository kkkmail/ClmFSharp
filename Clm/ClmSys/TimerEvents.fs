namespace ClmSys

open System.Threading
open System

module TimerEvents =

    [<Literal>]
    let RefreshInterval = 30_000


    [<Literal>]
    let OneHourRefreshInterval = 3_600_000


    type EventHandlerInfo =
        {
            eventHandler : unit -> unit
            refreshInterfal : int option
            logger : exn -> unit
        }

        static member defaultValue logger h =
            {
                eventHandler = h
                refreshInterfal = None
                logger = logger
            }

        static member oneHourValue logger h =
            {
                eventHandler = h
                refreshInterfal = Some OneHourRefreshInterval
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
                        i.eventHandler()
                    with
                    | e -> i.logger e
                else
                    printfn "!!! ERROR - EventHandler: %A is still running !!!" handlerId
                    ignore()
            finally
                Interlocked.Decrement(&counter) |> ignore


        let timer = new System.Timers.Timer(refreshInterfal)
        do timer.AutoReset <- true
        do timer.Elapsed.Add eventHandler

        member __.start() = do timer.Start()
