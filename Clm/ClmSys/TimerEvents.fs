﻿namespace ClmSys

open System.Threading
open System
open GeneralErrors
open ClmErrors
open Logging
open GeneralData

module TimerEvents =

    [<Literal>]
    let RefreshInterval = 30_000


    [<Literal>]
    let OneHourRefreshInterval = 3_600_000


    type ClmEventHandlerInfo =
        {
            handlerId : Guid option
            handlerName : string
            eventHandler : unit -> UnitResult
            refreshInterfal : int option
            logger : Logger
        }

        static member defaultValue logger h n =
            {
                handlerId = None
                handlerName = n
                eventHandler = h
                refreshInterfal = None
                logger = logger
            }

        static member oneHourValue logger h n =
            {
                handlerId = None
                handlerName = n
                eventHandler = h
                refreshInterfal = Some OneHourRefreshInterval
                logger = logger
            }


    type ClmEventHandler(i : ClmEventHandlerInfo) =
        let mutable counter = -1
        let handlerId = i.handlerId |> Option.defaultValue (Guid.NewGuid())
        let refreshInterfal = i.refreshInterfal |> Option.defaultValue RefreshInterval |> float
        let logError e = e |> ClmEventHandlerErr |> i.logger.logError
        let logWarn e = e |> ClmEventHandlerErr |> i.logger.logWarn
        let info = sprintf "ClmEventHandler: handlerId = %A, handlerName = %A" handlerId i.handlerName
        do info |> ClmInfo |> i.logger.logInfo

        let g() =
            try
                match i.eventHandler() with
                | Ok() -> ignore()
                | Error e -> i.logger.logError e
            with
            | e -> (i.handlerName, handlerId, e) |> UnhandledException |> logError

        let eventHandler _ =
            try
                if Interlocked.Increment(&counter) = 0
                then timedImpl logger info g
                else (i.handlerName, handlerId, DateTime.Now) |> StillRunningError |> logWarn
            finally Interlocked.Decrement(&counter) |> ignore


        let timer = new System.Timers.Timer(refreshInterfal)
        do timer.AutoReset <- true
        do timer.Elapsed.Add eventHandler

        member _.start() = do timer.Start()
        member _.stop() = do timer.Stop()
