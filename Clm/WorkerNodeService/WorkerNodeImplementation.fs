﻿namespace WorkerNodeService

open System
open Argu
open ClmSys.GeneralData
open ClmSys.Logging
open ClmSys.WorkerNodeData
open ClmSys.TimerEvents
open ClmSys.Registry
open ContGenServiceInfo.ServiceInfo
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.SvcCommandLine
open ServiceProxy.MsgServiceProxy
open MessagingServiceInfo.ServiceInfo
open Messaging.Client
open Messaging.ServiceResponse
open Clm.ModelParams
open ServiceProxy.WorkerNodeProxy
open Clm.CommandLine
open System.IO

module ServiceImplementation =

    let mutable serviceAccessInfo =
        let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = WorkerNodeServiceProgramName)
        let results = (parser.Parse [||]).GetAllResults()
        results |> getServiceAccessInfo


    type WorkerNodeRunnerState =
        {
            runningWorkers : Map<LocalProcessId, RemoteProcessId>
        }

        static member maxMessages = [ for _ in 1..maxNumberOfMessages -> () ]

        static member defaultValue =
            {
                runningWorkers = Map.empty
            }


    type WorkerNodeRunnerData =
        {
            workerNodeAccessInfo : WorkerNodeServiceAccessInfo
            msgResponseHandler : MsgResponseHandler
            msgClientProxy : MessagingClientProxy
            workerNodeProxy : WorkerNodeProxy
            logger : Logger
            exeName : string
            minUsefulEe : MinUsefulEe
        }

        member this.messagingClientData =
            {
                msgAccessInfo = this.workerNodeAccessInfo.workNodeMsgAccessInfo.messagingClientAccessInfo
                msgResponseHandler = this.msgResponseHandler
                msgClientProxy = this.msgClientProxy
                logger = this.logger
            }


    type WorkerNodeMessage =
        | Start
        | Register
        | Unregister
        | UpdateProgress of LocalProgressUpdateInfo
        | GetMessages
        | RunModel of WorkerNodeRunModelData
        | GetState of AsyncReplyChannel<WorkerNodeRunnerState>
        | ConfigureWorker of WorkerNodeConfigParam


    type WorkerNodeRunner(i : WorkerNodeRunnerData) =
        let messagingClient = MessagingClient i.messagingClientData
        do messagingClient.start()

        let partitioner = i.workerNodeAccessInfo.partitionerId
        let sendMessage m = messagingClient.sendMessage m
        let logErr = i.logger.logErr


        let getSolverRunnerAccessInfo ee =
            {
                wrkNodeServiceAccessInfo = i.workerNodeAccessInfo.workerNodeServiceAccessInfo
                minUsefulEe = ee
            }
            |> WorkerNodeSvcAccessInfo


        let onRunModel (s : WorkerNodeRunnerState) (d : WorkerNodeRunModelData) =
            printfn "WorkerNodeRunner.onRunModel: d = %A." d
            i.workerNodeProxy.saveWorkerNodeRunModelData d

            let a =
                {
                    exeName = i.exeName
                    commandLineParam =
                        {
                            taskParam = d.taskParam
                            serviceAccessInfo = getSolverRunnerAccessInfo d.minUsefulEe
                        }

                    callBackInfo =
                        {
                            modelDataId = d.modelDataId
                            runQueueId = d.runQueueId
                        }
                }

            match i.workerNodeProxy.runModel a with
            | Some result ->
                printfn "WorkerNodeRunner.onRunModel: Number of running models = %A." (s.runningWorkers.Count + 1)
                { s with runningWorkers = s.runningWorkers.Add(result.localProcessId, d.remoteProcessId) }
            | None -> s


        let onStart s =
            printfn "WorkerNodeRunner.onStart"
            i.workerNodeProxy.loadAllWorkerNodeRunModelData()
            |> List.fold (fun acc e -> onRunModel acc e) s


        let onRegister s =
            printfn "WorkerNodeRunner.onRegister"
            {
                partitionerRecipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = i.workerNodeAccessInfo.workerNodeInfo |> RegisterWorkerNodePrtMsg
            }.messageInfo
            |> sendMessage

            s


        let onUnregister s =
            printfn "WorkerNodeRunner.onUnregister"
            {
                partitionerRecipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = i.workerNodeAccessInfo.workerNodeInfo.workerNodeId |> UnregisterWorkerNodePrtMsg
            }.messageInfo
            |> sendMessage

            s


        let onSaveResult (d : ResultDataId) =
            printfn "WorkerNodeRunner.onSaveResult: d = %A." d
            match i.workerNodeProxy.tryLoadResultData d with
            | Some r ->
                {
                    partitionerRecipient = partitioner
                    deliveryType = GuaranteedDelivery
                    messageData = r |> SaveResultPrtMsg
                }.messageInfo
                |> sendMessage

                i.workerNodeProxy.tryDeleteResultData d |> ignore
            | None -> logErr (sprintf "Unable to find result with resultDataId: %A" d)


        let onSaveCharts (d : ResultDataId) =
            printfn "WorkerNodeRunner.onSaveCharts: d = %A." d
            match i.workerNodeProxy.tryLoadChartInfo d with
            | Some c ->
                {
                    partitionerRecipient = partitioner
                    deliveryType = GuaranteedDelivery
                    messageData = c |> SaveChartsPrtMsg
                        //{
                        //    c with charts = c.charts |> List.map (fun e -> { e with chartName = Path.GetFileNameWithoutExtension e.chartName })
                        //} |> SaveChartsPrtMsg
                }.messageInfo
                |> sendMessage

                try
                    c.charts
                    |> List.map (fun e -> if File.Exists e.chartName then File.Delete e.chartName)
                    |> ignore
                with
                    | ex ->
                        i.logger.logExn "onSaveCharts - Exception occurred:" ex

                i.workerNodeProxy.tryDeleteChartInfo d |> ignore
            | None -> logErr (sprintf "Unable to find charts with resultDataId: %A" d)


        let onUpdateProgress s (p : LocalProgressUpdateInfo) =
            printfn "WorkerNodeRunner.onUpdateProgress: p = %A." p
            let updateProgress t c =
                match s.runningWorkers.TryFind p.updatedLocalProcessId with
                | Some r ->
                    let q =
                        {
                            updatedRemoteProcessId = r
                            updateModelId = p.updateModelId
                            progress = p.progress
                            resultDataId = p.resultDataId
                        }
                    {
                        partitionerRecipient = partitioner
                        deliveryType = t
                        messageData = UpdateProgressPrtMsg q
                    }.messageInfo
                    |> sendMessage

                    if c
                    then
                        printfn "WorkerNodeRunner.onUpdateProgress: Calling tryDeleteWorkerNodeRunModelData and tryDeleteModelData..."
                        i.workerNodeProxy.tryDeleteWorkerNodeRunModelData r |> ignore
                        i.workerNodeProxy.tryDeleteModelData p.updateModelId |> ignore
                        onSaveResult p.resultDataId
                        onSaveCharts p.resultDataId
                | None -> logErr (sprintf "Unable to find mapping from local process %A." p.updatedLocalProcessId)

                if c
                then { s with runningWorkers = s.runningWorkers.tryRemove p.updatedLocalProcessId }
                else s


            let (t, c) =
                match p.progress with
                | NotStarted -> (NonGuaranteedDelivery, false)
                | InProgress _ -> (NonGuaranteedDelivery, false)
                | Completed -> (GuaranteedDelivery, true)

            updateProgress t c


        let tryFindRunningModel (s : WorkerNodeRunnerState) (d : WorkerNodeRunModelData) =
            s.runningWorkers
            |> Map.toList
            |> List.map (fun (_, v) -> v)
            |> List.tryFind (fun e -> e = d.remoteProcessId)


        let onProcessMessage s (m : Message) =
            printfn "WorkerNodeRunner.onProcessMessage: m.messageId = %A." m.messageId
            match m.messageInfo.messageData with
            | WorkerNodeMsg x ->
                match x with
                | RunModelWrkMsg (d, m) ->
                    i.workerNodeProxy.saveModelData m
                    match tryFindRunningModel s d with
                    | None ->
                        onRunModel s d
                    | Some r ->
                        printfn "WorkerNodeRunner.onProcessMessage: !!! ERROR !!! - found running model for remoteProcessId = %A" r.value
                        s
            | _ ->
                i.logger.logErr (sprintf "Invalid message type: %A." m.messageInfo.messageData)
                s


        let onGetMessages s =
            async {
                printfn "WorkerNodeRunner.onGetMessages"
                printfn "WorkerNodeRunnerState: %A" s
                return! List.foldWhileSomeAsync (fun x () -> messagingClient.tryProcessMessage x onProcessMessage) WorkerNodeRunnerState.maxMessages s
            }

        let onConfigureWorker s d =
            match d with
            | WorkerNumberOfSores c ->
                printfn "WorkerNodeRunner.onConfigureWorkers"
                let cores = max 0 (min c Environment.ProcessorCount)
                {
                    partitionerRecipient = partitioner
                    deliveryType = GuaranteedDelivery
                    messageData = { i.workerNodeAccessInfo.workerNodeInfo with noOfCores = cores} |> RegisterWorkerNodePrtMsg
                }.messageInfo
                |> sendMessage

            s


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start -> return! timed "WorkerNodeRunner.onStart" onStart s |> loop
                            | Register -> return! timed "WorkerNodeRunner.onRegister" onRegister s |> loop
                            | Unregister -> return! timed "WorkerNodeRunner.onUnregister" onUnregister s |> loop
                            | UpdateProgress p -> return! timed "WorkerNodeRunner.onUpdateProgress" onUpdateProgress s p |> loop
                            | GetMessages ->
                                let! ns = onGetMessages s
                                return! ns |> loop
                            | RunModel d -> return! timed "WorkerNodeRunner.onRunModel" onRunModel s d |> loop
                            | GetState w -> w.Reply s
                            | ConfigureWorker d -> return! timed "WorkerNodeRunner.onConfigureWorker" onConfigureWorker s d |> loop
                        }

                WorkerNodeRunnerState.defaultValue |> loop
                )

        member __.start() = Start |> messageLoop.Post
        member __.register() = Register |> messageLoop.Post
        member __.unregister() = Unregister |> messageLoop.Post
        member __.updateProgress p = UpdateProgress p |> messageLoop.Post
        member __.getMessages() = GetMessages |> messageLoop.Post
        member __.getState () = messageLoop.PostAndReply GetState
        member __.configure d = d |> ConfigureWorker |> messageLoop.Post


    let createServiceImpl i =
        printfn "createServiceImpl: Creating WorkerNodeRunner..."
        let w = WorkerNodeRunner i

        match i.workerNodeAccessInfo.isInactive with
        | false ->
            printfn "createServiceImpl: Registering..."
            do w.register()
            do w.start()
            let h = new EventHandler(EventHandlerInfo.defaultValue w.getMessages)
            do h.start()
            Some w
        | true ->
            printfn "createServiceImpl: Unregistering..."
            do w.unregister()
            do w.getState() |> ignore
            None


    type WorkerNodeService () =
        inherit MarshalByRefObject()

        let w =
            match MsgResponseHandler.tryCreate serviceAccessInfo.workNodeMsgAccessInfo.messagingClientAccessInfo with
            | Some h ->
                printfn "WorkerNodeService: Created MsgResponseHandler: %A" h
                {
                    workerNodeAccessInfo = serviceAccessInfo
                    msgResponseHandler = h
                    msgClientProxy = MessagingClientProxy { messagingClientName = workerNodeServiceName }
                    workerNodeProxy = WorkerNodeProxy WorkerNodeProxyInfo.defaultValue
                    logger = logger
                    exeName = SolverRunnerName
                    minUsefulEe = MinUsefulEe.defaultValue

                }
                |> createServiceImpl
            | None ->
                printfn "WorkerNodeService: Cannot create MsgResponseHandler."
                None

        let initService () = ()
        do initService ()


        let updateLocalProgressImpl p =
            match w with
            | Some r -> r.updateProgress p
            | None -> logger.logErr (sprintf "Failed to update progress: %A" p)


        let configureImpl d =
            match w with
            | Some r -> r.configure d
            | None -> logger.logErr (sprintf "Failed to configure service: %A" d)


        interface IWorkerNodeService with
            member __.updateLocalProgress p = updateLocalProgressImpl p
            member __.ping() = ignore()
            member __.configure d = configureImpl d
