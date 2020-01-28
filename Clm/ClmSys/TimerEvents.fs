namespace ClmSys

open System.Threading
open System
open GeneralErrors
open ClmErrors

module TimerEvents =

    [<Literal>]
    let RefreshInterval = 30_000


    [<Literal>]
    let OneHourRefreshInterval = 3_600_000


    type ClmEventHandlerInfo =
        {
            handlerId : Guid option
            eventHandler : unit -> UnitResult
            refreshInterfal : int option
            logError : ClmError -> unit
        }

        static member defaultValue logError h =
            {
                handlerId = None
                eventHandler = h
                refreshInterfal = None
                logError = logError
            }

        static member oneHourValue logError h =
            {
                handlerId = None
                eventHandler = h
                refreshInterfal = Some OneHourRefreshInterval
                logError = logError
            }


    type ClmEventHandler(i : ClmEventHandlerInfo) =
        let mutable counter = -1
        let handlerId = i.handlerId |> Option.defaultValue (Guid.NewGuid())
        let refreshInterfal = i.refreshInterfal |> Option.defaultValue RefreshInterval |> float
        let logError e = e |> ClmEventHandlerErr |> i.logError

        let eventHandler _ =
            try
                if Interlocked.Increment(&counter) = 0
                then
                    try
                        match i.eventHandler() with
                        | Ok() -> ignore()
                        | Error e -> i.logError e
                    with
                    | e -> (handlerId, e) |> UnhandledException |> logError
                else handlerId |> StillRunningError |> logError
            finally
                Interlocked.Decrement(&counter) |> ignore


        let timer = new System.Timers.Timer(refreshInterfal)
        do timer.AutoReset <- true
        do timer.Elapsed.Add eventHandler

        member _.start() = do timer.Start()
