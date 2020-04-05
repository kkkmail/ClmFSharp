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
                let q1 = { q with workerNodeIdOpt = Some n; runQueueStatus = InProgressRunQueue }

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
                | NotStartedRunQueue | InProgressRunQueue -> { r with runQueueStatus = CancelledRunQueue } |> proxy.upsertRunQueue
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
        printfn "updateProgress: i = %A" i
        let addError = addError UpdateProgressErr
        let toError = toError UpdateProgressErr

        match proxy.tryLoadRunQueue i.runQueueId with
        | Ok (Some q) ->
            match q.runQueueStatus with
            | InProgressRunQueue ->
                let q1 = { q with progress = i.progress }

                let q2 =
                    match i.progress with
                    | NotStarted | InProgress _ -> q1
                    | Completed _ -> { q1 with runQueueStatus = CompletedRunQueue; errorMessageOpt = None }
                    | Failed e -> { q1 with runQueueStatus = FailedRunQueue; errorMessageOpt = Some e }

                match proxy.upsertRunQueue q2 with
                | Ok() -> Ok()
                | Error e -> addError (UnableToLoadRunQueueErr i.runQueueId) e
            | NotStartedRunQueue | InactiveRunQueue | CompletedRunQueue | FailedRunQueue | ModifyingRunQueue | CancelledRunQueue ->
                toError (InvalidRunQueueStatusErr i.runQueueId)
            | InvalidRunQueue -> toError (CompleteyInvalidRunQueueStatusErr i.runQueueId)
        | Ok None -> toError (UnableToFindLoadRunQueueErr i.runQueueId)
        | Error e -> addError (UnableToLoadRunQueueErr i.runQueueId) e


    let register (proxy : RegisterProxy) (r : WorkerNodeInfo) =
        printfn "register: r = %A" r
        proxy.upsertWorkerNodeInfo r |> bindError (addError RegisterErr (UnableToUpsertWorkerNodeInfoErr r.workerNodeId))


    let unregister (proxy : UnregisterProxy) (r : WorkerNodeId) =
        printfn "unregister: r = %A" r
        let addError = addError UnregisterErr

        match proxy.loadWorkerNodeInfo r with
        | Ok w -> proxy.upsertWorkerNodeInfo { w with noOfCores = 0 } |> bindError (addError (UnableToUpsertWorkerNodeInfoOnUnregisterErr r))
        | Error e -> addError (UnableToLoadWorkerNodeInfoErr r) e


    let saveResult (proxy : SaveResultProxy) r =
        printfn "updateProgress: r= %A" r
        proxy.saveResultData r |> bindError (addError SaveResultErr (UnableToSaveResultDataErr r.resultDataId))


    let saveCharts (proxy : SaveChartsProxy) c =
        printfn "saveResult: c.resultDataId = %A" c.resultDataId
        proxy.saveCharts c |> bindError (addError SaveChartsErr (UnableToSaveCharts c.resultDataId))


    let processMessage (proxy : ProcessMessageProxy) (m : Message) =
        printfn "processMessage: messageId = %A" m.messageDataInfo.messageId
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


    let createModelRunner (logger : Logger) c rmp =
        logger.logInfoString "createModelRunner: Creating model runner..."
        let proxy = TryRunAllModelsProxy.create c rmp
        let e = fun () -> tryRunAllModels proxy
        let h = new ClmEventHandler(ClmEventHandlerInfo.defaultValue logger e "ModelRunner - tryRunAllModels")
        h


    let createModelRunnerMessageProcessor (logger : Logger) c resultLocation w =
        logger.logInfoString "createModelRunnerMessageProcessor: Creating message procesor..."
        let proxy = onGetMessagesProxy c resultLocation w
        let e = fun () -> onGetMessages proxy () |> snd
        let h = new ClmEventHandler(ClmEventHandlerInfo.defaultValue logger e "ModelRunnerMessageProcessor - onGetMessages")
        h


    type ModelRunnerData =
        {
            connectionString : ConnectionString
            minUsefulEe : MinUsefulEe
            resultLocation : string
        }


    type ModelRunner =
        {
            modelGenerator : ClmEventHandler
            modelRunner : ClmEventHandler
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

        static member create (logger : Logger) (d : ModelRunnerData) (w : MessageProcessorProxy) =
            let rmp = RunModelProxy.create d.connectionString d.minUsefulEe w.sendMessage

            {
                modelGenerator = createModelGenerator logger d.connectionString
                modelRunner = createModelRunner logger d.connectionString rmp
                messageProcessor = createModelRunnerMessageProcessor logger d.connectionString d.resultLocation w
            }


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


    //type ModelConfigurator =
    //    {
    //        x : int
    //    }

    //    static member create connectionString =
    //        let proxy = TryCancelRunQueueProxy.create connectionString

    //        0
