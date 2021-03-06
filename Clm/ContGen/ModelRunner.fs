﻿namespace ContGen

open Clm.ModelParams
open ClmSys.SolverRunnerPrimitives
open ContGenServiceInfo.ServiceInfo
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.WorkerNodeData
open ClmSys.Rop
open ServiceProxy.ModelRunnerProxy
open ClmSys.ModelRunnerErrors
open MessagingServiceInfo.ServiceInfo
open ClmSys.WorkerNodePrimitives
open ClmSys.Logging
open ClmSys.TimerEvents
open DbData.DatabaseTypes
open ServiceProxy.MsgProcessorProxy
open Messaging.Client
open ModelGenerator
open ClmSys.MessagingData

module ModelRunner =

    let private toError g f = f |> g |> ModelRunnerErr |> Error
    let private addError g f e = ((f |> g |> ModelRunnerErr) + e) |> Error
    let private maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]

    type OnProcessMessageType = OnProcessMessageType<unit>
    type OnGetMessagesProxy = OnGetMessagesProxy<unit>
    let onGetMessages = onGetMessages<unit>


    let runModel (proxy : RunModelProxy) (q : RunQueue) =
        match q.toMessageInfoOpt proxy.loadModelData proxy.minUsefulEe proxy.earlyExitInfo with
        | Ok (Some m) -> proxy.sendRunModelMessage m
        | Ok None -> q.runQueueId |> MissingWorkerNodeErr |> toError RunModelErr
        | Error e -> (addError RunModelErr) (UnableToLoadModelDataErr (q.runQueueId, q.info.modelDataId )) e


    /// Tries to run the first not scheduled run queue entry using the first available worker node.
    let tryRunFirstModel (proxy : TryRunFirstModelProxy) =
        let addError = addError TryRunFirstModelErr

        match proxy.tryLoadFirstRunQueue() with
        | Ok (Some q) ->
            match proxy.tryGetAvailableWorkerNode() with
            | Ok (Some n) ->
                let q1 = { q with workerNodeIdOpt = Some n; runQueueStatus = RunRequestedRunQueue }

                match proxy.upsertRunQueue q1 with
                | Ok() ->
                    match proxy.runModel q1 with
                    | Ok() -> Ok WorkScheduled
                    | Error e ->
                        match proxy.upsertRunQueue { q1 with runQueueStatus = FailedRunQueue } with
                        | Ok() -> addError UnableToRunModelErr e
                        | Error f -> addError UnableToRunModelAndUpsertStatusErr (f + e)
                | Error e -> addError UpsertRunQueueErr e
            | Ok None -> Ok NoAvailableWorkerNodes
            | Error e -> addError TryGetAvailableWorkerNodeErr e
        | Ok None -> Ok NoWork
        | Error e -> addError TryLoadFirstRunQueueErr e


    let tryCancelRunQueue (proxy : TryCancelRunQueueProxy) (q, c) =
        let addError = addError TryCancelRunQueueErr
        let toError = toError TryCancelRunQueueErr

        match proxy.tryLoadRunQueue q with
        | Ok (Some r) ->
            let r1 =
                match r.workerNodeIdOpt with
                | Some w ->
                    {
                        recipientInfo =
                            {
                                recipient = w.messagingClientId
                                deliveryType = GuaranteedDelivery
                            }

                        messageData = (q, c) |> CancelRunWrkMsg |> WorkerNodeMsg
                    }
                    |> proxy.sendCancelRunQueueMessage
                | None -> Ok()

            let r2 =
                match r.runQueueStatus with
                | NotStartedRunQueue -> { r with runQueueStatus = CancelledRunQueue } |> proxy.upsertRunQueue
                | RunRequestedRunQueue -> { r with runQueueStatus = CancelRequestedRunQueue } |> proxy.upsertRunQueue
                | InProgressRunQueue -> { r with runQueueStatus = CancelRequestedRunQueue } |> proxy.upsertRunQueue
                | CancelRequestedRunQueue -> { r with runQueueStatus = CancelRequestedRunQueue } |> proxy.upsertRunQueue
                | _ -> q |> TryCancelRunQueueError.InvalidRunQueueStatusErr |> toError

            combineUnitResults r1 r2
        | Ok None -> toError (TryCancelRunQueueError.TryLoadRunQueueErr q)
        | Error e -> addError (TryCancelRunQueueError.TryLoadRunQueueErr q) e


    let tryRequestResults (proxy : TryRequestResultsProxy) (q, c) =
        let addError = addError TryRequestResultsErr
        let toError = toError TryRequestResultsErr

        match proxy.tryLoadRunQueue q with
        | Ok (Some r) ->
            match r.workerNodeIdOpt with
            | Some w ->
                {
                    recipientInfo =
                        {
                            recipient = w.messagingClientId
                            deliveryType = GuaranteedDelivery
                        }

                    messageData = (q, c) |> RequestResultWrkMsg |> WorkerNodeMsg
                }
                |> proxy.sendRequestResultsMessage
            | None -> Ok()
        | Ok None -> toError (TryRequestResultsError.TryLoadRunQueueErr q)
        | Error e -> addError (TryRequestResultsError.TryLoadRunQueueErr q) e

    /// Tries to run all available work items (run queue) on all available work nodes until one or the other is exhausted.
    let tryRunAllModels (proxy : TryRunAllModelsProxy) =
        let rec doWork() =
            match proxy.tryRunFirstModel() with
            | Ok r ->
                match r with
                | WorkScheduled -> doWork()
                | NoWork -> Ok()
                | NoAvailableWorkerNodes -> Ok()
            | Error e -> addError TryRunAllModelsErr UnableToTryRunFirstModelErr e

        doWork()


    let updateProgress (proxy : UpdateProgressProxy) (i : ProgressUpdateInfo) =
        //printfn "updateProgress: i = %A" i
        let addError = addError UpdateProgressErr
        let toError = toError UpdateProgressErr

        match proxy.tryLoadRunQueue i.runQueueId with
        | Ok (Some q) ->
            let q1 = { q with progress = i.progress }

            let upsert (q2, r) =
                printfn "updateProgress.upsert: Upserting %A into %A." i q2

                match proxy.upsertRunQueue q2 with
                | Ok() -> r
                | Error e -> (r, addError (UnableToLoadRunQueueErr i.runQueueId) e) ||> combineUnitResults

            match i.progress with
            | NotStarted -> { q1 with runQueueStatus = NotStartedRunQueue; errorMessageOpt = None }, Ok()
            | InProgress _ ->{ q1 with runQueueStatus = InProgressRunQueue; errorMessageOpt = None }, Ok()
            | Completed v ->
                match v with
                | None, None -> { q1 with runQueueStatus = CompletedRunQueue; errorMessageOpt = None }, Ok()
                | Some d, None -> { q1 with runQueueStatus = CompletedRunQueue; errorMessageOpt = sprintf "The run queue was cancelled at: %.2f%% progress." (d * 100.0m) |> ErrorMessage |> Some }, Ok()
                | None, Some s -> { q1 with runQueueStatus = CompletedRunQueue; errorMessageOpt = sprintf "Message: %s" s |> ErrorMessage |> Some }, Ok()
                | Some d, Some s ->
                    let m = sprintf "The run queue was cancelled at: %.2f%% progress. Message: %s" (d * 100.0m) s
                    { q1 with runQueueStatus = CompletedRunQueue; errorMessageOpt = m |> ErrorMessage |> Some }, Ok()
            | Failed e -> { q1 with runQueueStatus = FailedRunQueue; errorMessageOpt = Some e }, Ok()
            | Cancelled v ->
                match v with
                | Some s -> { q1 with runQueueStatus = CancelledRunQueue; errorMessageOpt = (sprintf "The run queue was aborted. Message %s" s) |> ErrorMessage |> Some }, Ok()
                | None -> { q1 with runQueueStatus = CancelledRunQueue; errorMessageOpt = "The run queue was aborted." |> ErrorMessage |> Some }, Ok()
            | AllCoresBusy w ->
                let e = sprintf "Node %A is busy" w |> ErrorMessage |> Some
                { q1 with runQueueStatus = NotStartedRunQueue; workerNodeIdOpt = None; progress = NotStarted; errorMessageOpt = e }, proxy.upsertWorkerNodeErr w
            |> upsert
        | Ok None -> toError (UnableToFindLoadRunQueueErr i.runQueueId)
        | Error e -> addError (UnableToLoadRunQueueErr i.runQueueId) e


    let register (proxy : RegisterProxy) (r : WorkerNodeInfo) =
        //printfn "register: r = %A" r
        proxy.upsertWorkerNodeInfo r |> bindError (addError RegisterErr (UnableToUpsertWorkerNodeInfoErr r.workerNodeId))


    let unregister (proxy : UnregisterProxy) (r : WorkerNodeId) =
        //printfn "unregister: r = %A" r
        let addError = addError UnregisterErr

        match proxy.loadWorkerNodeInfo r with
        | Ok w -> proxy.upsertWorkerNodeInfo { w with noOfCores = 0 } |> bindError (addError (UnableToUpsertWorkerNodeInfoOnUnregisterErr r))
        | Error e -> addError (UnableToLoadWorkerNodeInfoErr r) e


    let saveResult (proxy : SaveResultProxy) (r : ResultDataWithId) =
        printfn "saveResult: resultDataId = %A" r.resultDataId
        proxy.saveResultData r |> bindError (addError SaveResultErr (UnableToSaveResultDataErr r.resultDataId))


    let saveCharts (proxy : SaveChartsProxy) (c : ChartInfo) =
        printfn "saveCharts: c.resultDataId = %A" c.resultDataId
        proxy.saveCharts c |> bindError (addError SaveChartsErr (UnableToSaveCharts c.resultDataId))


    let processMessage (proxy : ProcessMessageProxy) (m : Message) =
        printfn "processMessage: messageId = %A, message = %A" m.messageDataInfo.messageId m

        match m.messageData with
        | PartitionerMsg x ->
            match x with
            | UpdateProgressPrtMsg i -> proxy.updateProgress i
            | SaveResultPrtMsg r -> proxy.saveResult r
            | SaveChartsPrtMsg c -> proxy.saveCharts c
            | RegisterWorkerNodePrtMsg r -> proxy.register r
            | UnregisterWorkerNodePrtMsg r -> proxy.unregister r
            |> bindError (addError ProcessMessageErr (ErrorWhenProcessingMessageErr m.messageDataInfo.messageId))
        | _ -> toError ProcessMessageErr (InvalidMessageTypeErr m.messageDataInfo.messageId)


    let getRunState (proxy : GetRunStateProxy) =
        let w, e = proxy.loadRunQueueProgress() |> unzipListResult
        w, e |> foldToUnitResult


    type ProcessMessageProxy
        with
        static member create c resultLocation =
            {
                updateProgress = updateProgress (UpdateProgressProxy.create c)
                saveResult = saveResult (SaveResultProxy.create c)
                saveCharts = saveCharts (SaveChartsProxy.create resultLocation)
                register = register (RegisterProxy.create c)
                unregister = unregister (UnregisterProxy.create c)
            }


    type TryRunFirstModelProxy
        with
        static member create c rmp m =
            {
                tryLoadFirstRunQueue = fun () -> tryLoadFirstRunQueue c
                tryGetAvailableWorkerNode = fun () -> tryGetAvailableWorkerNode c m
                runModel = runModel rmp
                upsertRunQueue = upsertRunQueue c
            }


    type TryCancelRunQueueProxy
        with
        static member create c s =
            {
                tryLoadRunQueue = tryLoadRunQueue c
                sendCancelRunQueueMessage = s
                upsertRunQueue = upsertRunQueue c
            }


    type TryRequestResultsProxy
        with
        static member create c s =
            {
                tryLoadRunQueue = tryLoadRunQueue c
                sendRequestResultsMessage = s
            }


    type TryRunAllModelsProxy
        with
        static member create c r m =
            {
                tryRunFirstModel = fun () -> tryRunFirstModel (TryRunFirstModelProxy.create c r m)
            }


    let onGetMessagesProxy c resultLocation w =
        let proxy = ProcessMessageProxy.create c resultLocation
        let p () m = (), processMessage proxy m

        {
            tryProcessMessage = onTryProcessMessage w
            onProcessMessage = p
            maxMessages = maxMessages
            onError = fun f -> f |> OnGetMessagesErr |> ProcessMessageErr |> ModelRunnerErr
        }


    type RunnerProxy =
        {
            getMessageProcessorProxy : MessagingClientAccessInfo -> MessageProcessorProxy
            createMessagingEventHandlers : Logger -> MessageProcessorProxy -> unit
        }


    type RunnerData =
        {
            getConnectionString : unit -> ConnectionString
            minUsefulEe : MinUsefulEe
            resultLocation : string
            earlyExitInfoOpt : EarlyExitInfo option
            lastAllowedNodeErr : LastAllowedNodeErr
        }


    type RunModelProxy
        with
        static member create (d : RunnerData) s =
            {
                minUsefulEe = d.minUsefulEe
                sendRunModelMessage = s
                loadModelData = loadModelData d.getConnectionString
                earlyExitInfo = d.earlyExitInfoOpt
            }


    type RunnerDataWithProxy =
        {
            runnerData : RunnerData
            messageProcessorProxy : MessageProcessorProxy
        }


    type ModelRunnerDataWithProxy =
        {
            runnerData : RunnerData
            runnerProxy : RunnerProxy
            messagingClientAccessInfo : MessagingClientAccessInfo
            logger : Logger
        }


    type RunnerMessage =
        | TryRunAll of AsyncReplyChannel<UnitResult>
        | TryCancelRunQueue of AsyncReplyChannel<UnitResult> * RunQueueId * CancellationType
        | TryRequestResults of AsyncReplyChannel<UnitResult> * RunQueueId * ResultNotificationType
        | ProcessMessages of AsyncReplyChannel<UnitResult>


    type Runner (i : RunnerDataWithProxy) =
        let runModelProxy = RunModelProxy.create i.runnerData i.messageProcessorProxy.sendMessage
        let tryRunAllModelsProxy = TryRunAllModelsProxy.create i.runnerData.getConnectionString runModelProxy i.runnerData.lastAllowedNodeErr
        let tryCancelRunQueueProxy = TryCancelRunQueueProxy.create i.runnerData.getConnectionString i.messageProcessorProxy.sendMessage
        let tryRequestResultsProxy = TryRequestResultsProxy.create i.runnerData.getConnectionString i.messageProcessorProxy.sendMessage
        let proxy = onGetMessagesProxy i.runnerData.getConnectionString i.runnerData.resultLocation i.messageProcessorProxy

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop() =
                    async
                        {
                            match! u.Receive() with
                            | TryRunAll r -> tryRunAllModels tryRunAllModelsProxy |> r.Reply
                            | TryCancelRunQueue (r, q, c) -> tryCancelRunQueue tryCancelRunQueueProxy (q, c) |> r.Reply
                            | TryRequestResults (r, q, c) -> tryRequestResults tryRequestResultsProxy (q, c) |> r.Reply
                            | ProcessMessages r -> onGetMessages proxy () |> snd |> r.Reply

                            return! loop()
                        }

                loop()
                )

        member _.tryRunAll() = messageLoop.PostAndReply (fun reply -> TryRunAll reply)
        member _.tryCancelRunQueue q c = messageLoop.PostAndReply (fun reply -> TryCancelRunQueue (reply, q, c))
        member _.processMessages() = messageLoop.PostAndReply (fun reply -> ProcessMessages reply)
        member _.tryRequestResults q c = messageLoop.PostAndReply (fun reply -> TryRequestResults (reply, q, c))


    let createModelRunner (logger : Logger) (r : Runner) =
        logger.logInfoString "createModelRunner: Creating model runner..."
        let e = fun () -> r.tryRunAll()
        let h = new ClmEventHandler(ClmEventHandlerInfo.defaultValue logger e "ModelRunner - tryRunAllModels")
        h


    /// Call this function to create timer events necessary for automatic ModelRunner operation.
    /// If you don't call it, then you have to operate it by hands.
    let createModelRunnerMessageProcessor (logger : Logger) (r : Runner) =
        logger.logInfoString "createModelRunnerMessageProcessor: Creating message processor..."
        let e = fun () -> r.processMessages()
        let h = new ClmEventHandler(ClmEventHandlerInfo.defaultValue logger e "ModelRunnerMessageProcessor - onGetMessages")
        h


    type ModelRunner =
        {
            modelGenerator : ClmEventHandler
            modelRunner : ClmEventHandler
            tryCancelRunQueue : RunQueueId -> CancellationType -> UnitResult
            tryRequestResults: RunQueueId -> ResultNotificationType -> UnitResult
            messageProcessor : ClmEventHandler
        }

        member p.start() =
            do
                p.modelGenerator.start()
                p.modelRunner.start()
                p.messageProcessor.start()

        member p.stop() =
            do
                p.modelGenerator.stop()
                p.modelRunner.stop()
                p.messageProcessor.stop()

        static member create (d : ModelRunnerDataWithProxy) =
            let messagingClient = d.runnerProxy.getMessageProcessorProxy d.messagingClientAccessInfo

            match messagingClient.start() with
            | Ok() ->
                d.runnerProxy.createMessagingEventHandlers d.logger messagingClient
                let data =
                    {
                        runnerData = d.runnerData
                        messageProcessorProxy = messagingClient
                    }

                let runner = new Runner(data)

                {
                    modelGenerator = createModelGenerator d.logger d.runnerData.getConnectionString
                    modelRunner = createModelRunner d.logger runner
                    tryCancelRunQueue = runner.tryCancelRunQueue
                    tryRequestResults = runner.tryRequestResults
                    messageProcessor = createModelRunnerMessageProcessor d.logger runner
                }
                |> Ok
            | Error e -> Error e


    type ModelMonitor =
        {
            getRunState : unit -> (list<RunQueue> * UnitResult)
        }

        static member create connectionString =
            let proxy = GetRunStateProxy.create connectionString
            let g() = getRunState proxy

            {
                getRunState = g
            }
