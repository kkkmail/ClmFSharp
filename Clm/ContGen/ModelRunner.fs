namespace ContGen

open Clm.ModelParams
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
        match q.toMessageInfoOpt proxy.loadModelData proxy.minUsefulEe with
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


    let tryCancelRunQueue (proxy : TryCancelRunQueueProxy) q =
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

                        messageData = q |> CancelRunWrkMsg |> WorkerNodeMsg
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
        | Ok None -> toError (TryLoadRunQueueErr q)
        | Error e -> addError (TryLoadRunQueueErr q) e


    /// Tries to run all available work items (run queue) on all availalble work nodes until one or the other is exhausted.
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
                match proxy.upsertRunQueue q2 with
                | Ok() -> r
                | Error e -> (r, addError (UnableToLoadRunQueueErr i.runQueueId) e) ||> combineUnitResults

            match i.progress with
            | NotStarted | InProgress _ -> q1, Ok()
            | Completed _ -> { q1 with runQueueStatus = CompletedRunQueue; errorMessageOpt = None }, Ok()
            | Failed e -> { q1 with runQueueStatus = FailedRunQueue; errorMessageOpt = Some e }, Ok()
            | Cancelled -> { q1 with runQueueStatus = CancelledRunQueue; errorMessageOpt = "The run queue was cancelled." |> ErrorMessage |> Some }, Ok()
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


    let saveResult (proxy : SaveResultProxy) r =
        //printfn "saveResult: r= %A" r
        proxy.saveResultData r |> bindError (addError SaveResultErr (UnableToSaveResultDataErr r.resultDataId))


    let saveCharts (proxy : SaveChartsProxy) c =
        //printfn "saveCharts: c.resultDataId = %A" c.resultDataId
        proxy.saveCharts c |> bindError (addError SaveChartsErr (UnableToSaveCharts c.resultDataId))


    let processMessage (proxy : ProcessMessageProxy) (m : Message) =
        //printfn "processMessage: messageId = %A" m.messageDataInfo.messageId
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

        static member create c rmp =
            {
                tryLoadFirstRunQueue = fun () -> tryLoadFirstRunQueue c
                tryGetAvailableWorkerNode = fun () -> tryGetAvailableWorkerNode c
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


    type TryRunAllModelsProxy
        with

        static member create c r =
            {
                tryRunFirstModel = fun () -> tryRunFirstModel (TryRunFirstModelProxy.create c r)
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
            connectionString : ConnectionString
            minUsefulEe : MinUsefulEe
            resultLocation : string
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
        | TryCancelRunQueue of AsyncReplyChannel<UnitResult> * RunQueueId
        | ProcessMessages of AsyncReplyChannel<UnitResult>


    type Runner (i : RunnerDataWithProxy) =
        let runModelProxy = RunModelProxy.create i.runnerData.connectionString i.runnerData.minUsefulEe i.messageProcessorProxy.sendMessage
        let tryRunAllModelsProxy = TryRunAllModelsProxy.create i.runnerData.connectionString runModelProxy
        let tryCancelRunQueueProxy = TryCancelRunQueueProxy.create i.runnerData.connectionString i.messageProcessorProxy.sendMessage
        let proxy = onGetMessagesProxy i.runnerData.connectionString i.runnerData.resultLocation i.messageProcessorProxy

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop() =
                    async
                        {
                            match! u.Receive() with
                            | TryRunAll r -> tryRunAllModels tryRunAllModelsProxy |> r.Reply
                            | TryCancelRunQueue (r, q) -> tryCancelRunQueue tryCancelRunQueueProxy q |> r.Reply
                            | ProcessMessages r -> onGetMessages proxy () |> snd |> r.Reply

                            return! loop()
                        }

                loop()
                )

        member _.tryRunAll() = messageLoop.PostAndReply (fun reply -> TryRunAll reply)
        member _.tryCancelRunQueue q = messageLoop.PostAndReply (fun reply -> TryCancelRunQueue (reply, q))
        member _.processMessages() = messageLoop.PostAndReply (fun reply -> ProcessMessages reply)


    let createModelRunner (logger : Logger) (r : Runner) =
        logger.logInfoString "createModelRunner: Creating model runner..."
        let e = fun () -> r.tryRunAll()
        let h = new ClmEventHandler(ClmEventHandlerInfo.defaultValue logger e "ModelRunner - tryRunAllModels")
        h


    /// Call this function to create timer events necessary for automatic ModelRunner operation.
    /// If you don't call it, then you have to operate it by hands.
    let createModelRunnerMessageProcessor (logger : Logger) (r : Runner) =
        logger.logInfoString "createModelRunnerMessageProcessor: Creating message procesor..."
        let e = fun () -> r.processMessages()
        let h = new ClmEventHandler(ClmEventHandlerInfo.defaultValue logger e "ModelRunnerMessageProcessor - onGetMessages")
        h


    type ModelRunner =
        {
            modelGenerator : ClmEventHandler
            modelRunner : ClmEventHandler
            tryCancelRunQueue : RunQueueId -> UnitResult
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
                    modelGenerator = createModelGenerator d.logger d.runnerData.connectionString
                    modelRunner = createModelRunner d.logger runner
                    tryCancelRunQueue = runner.tryCancelRunQueue
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
