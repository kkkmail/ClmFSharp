namespace WorkerNodeService

open System
open Argu
open ClmSys.GeneralData
open ClmSys.Logging
open ClmSys.WorkerNodeData
open ClmSys.TimerEvents
open Clm.CalculationData
open ContGenServiceInfo.ServiceInfo
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.SvcCommandLine
open MessagingServiceInfo.ServiceProxy
open MessagingServiceInfo.ServiceInfo
open Messaging.Client
open Messaging.ServiceResponse
open Clm.ModelParams
open ServiceProxy.WorkerNodeProxy
open Clm.CommandLine

module ServiceImplementation =

    let mutable serviceAccessInfo =
        let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = WorkerNodeServiceProgramName)
        let results = (parser.Parse [||]).GetAllResults()
        results |> getServiceAccessInfo


    type WorkerNodeRunnerState =
        {
            running : Map<LocalProcessId, RemoteProcessId>
        }

        static member defaultValue =
            {
                running = Map.empty
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
        | UpdateProgress of LocalProgressUpdateInfo
        | SaveResult of ResultDataWithId
        | SaveCharts of ChartInfo
        | GetMessages of WorkerNodeRunner
        | ProcessMessage of WorkerNodeRunner * Message
        | RunModel of WorkerNodeRunner * WorkerNodeRunModelData


    and WorkerNodeRunner(i : WorkerNodeRunnerData) =
        let messagingClient = MessagingClient i.messagingClientData
        let partitioner = i.workerNodeAccessInfo.partitionerId
        let sendMessage m = messagingClient.sendMessage m
        let logErr = i.logger.logErr


        let getSolverRunnerAccessInfo ee =
            {
                wrkNodeServiceAccessInfo = i.workerNodeAccessInfo.workerNodeServiceAccessInfo
                minUsefulEe = ee
            }
            |> WorkerNodeSvcAccessInfo


        /// TODO kk:20190819 - Load:
        ///     1. Messages
        ///     2. What to run.
        ///     3. Etc...
        let onStart s =
            s


        let onRegister s =
            {
                partitionerRecipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = i.workerNodeAccessInfo.workerNodeInfo |> RegisterWorkerNodePrtMsg
            }.messageInfo
            |> sendMessage

            s


        let onUpdateProgress s (p : LocalProgressUpdateInfo) =
            let updateProgress t c =
                match s.running.TryFind p.updatedLocalProcessId with
                | Some r ->
                    let q =
                        {
                            updatedRemoteProcessId = r
                            updateModelId = p.updateModelId
                            progress = p.progress
                        }
                    {
                        partitionerRecipient = partitioner
                        deliveryType = t
                        messageData = UpdateProgressPrtMsg q
                    }.messageInfo
                    |> sendMessage

                    i.workerNodeProxy.tryDeleteWorkerNodeRunModelData r |> ignore
                | None -> logErr (sprintf "Unable to find mapping from local process %A." p.updatedLocalProcessId)

                if c
                then { s with running = s.running.tryRemove p.updatedLocalProcessId }
                else s


            let (t, c) =
                match p.progress with
                | NotStarted -> (NonGuaranteedDelivery, false)
                | InProgress _ -> (NonGuaranteedDelivery, false)
                | Completed -> (GuaranteedDelivery, true)

            updateProgress t c


        let onSaveResult s r =
            {
                partitionerRecipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = r |> SaveResultPrtMsg
            }.messageInfo
            |> sendMessage

            s


        let onSaveCharts s c =
            {
                partitionerRecipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = c |> SaveChartsPrtMsg
            }.messageInfo
            |> sendMessage

            s


        let onGetMessages s (r : WorkerNodeRunner) =
            let messages = messagingClient.getMessages()

            messages
            |> List.filter (fun e -> match e.messageInfo.deliveryType with | GuaranteedDelivery -> true | NonGuaranteedDelivery -> false)
            |> List.map (fun e -> i.msgClientProxy.saveMessage IncomingMessage e)
            |> ignore

            messages
            |> List.map (fun e -> r.processMessage e)
            |> ignore

            s


        let onRunModel (s : WorkerNodeRunnerState) (r : WorkerNodeRunner) (m : WorkerNodeRunModelData) =
            i.workerNodeProxy.saveWorkerNodeRunModelData m

            let a =
                {
                    runModelParam =
                        {
                            exeName = i.exeName
                            commandLineParam =
                                {
                                    taskParam = m.taskParam
                                    serviceAccessInfo = getSolverRunnerAccessInfo m.minUsefulEe
                                }
                        }

                    callBackInfo =
                        {
                            processStartedInfo =
                                {
                                    calledBackModelId = m.wrkModelData.modelDataId
                                    runQueueId = m.runQueueId
                                }

                            callBack =
                                {
                                    notifyOnStarted = r.onStarted
                                }
                        }
                }

            let result = i.workerNodeProxy.runModel a
            { s with running = s.running.Add(result.localProcessId, m.remoteProcessId) }


        let onProcessMessage s (w : WorkerNodeRunner) (m : Message) =
            match m.messageInfo.messageData with
            | WorkerNodeMsg x ->
                match x with
                | RunModelWrkMsg m -> w.runModel m
            | _ -> i.logger.logErr (sprintf "Invalid message type: %A." m.messageInfo.messageData)

            match m.messageInfo.deliveryType with
            | GuaranteedDelivery -> i.msgClientProxy.deleteMessage m.messageId
            | NonGuaranteedDelivery -> ignore()

            s


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start -> return! onStart s |> loop
                            | Register -> return! onRegister s |> loop
                            | UpdateProgress p -> return! onUpdateProgress s p |> loop
                            //| SaveModelData m -> return! onSaveModelData s m |> loop
                            | SaveResult r -> return! onSaveResult s r |> loop
                            | SaveCharts c -> return! onSaveCharts s c |> loop
                            | GetMessages w -> return! onGetMessages s w |> loop
                            | ProcessMessage (w, m) -> return! onProcessMessage s w m |> loop
                            | RunModel (w, m) -> return! onRunModel s w m |> loop
                        }

                onStart (WorkerNodeRunnerState.defaultValue) |> loop
                )

        member __.start() = Start |> messageLoop.Post
        member __.register() = Register |> messageLoop.Post
        member __.updateProgress p = UpdateProgress p |> messageLoop.Post
        //member __.saveModelData m = SaveModelData m |> messageLoop.Post
        member __.saveCharts c = SaveCharts c |> messageLoop.Post
        member this.getMessages() = GetMessages this |> messageLoop.Post
        member private this.processMessage m = ProcessMessage (this, m) |> messageLoop.Post
        member private this.runModel m = RunModel (this, m) |> messageLoop.Post
        member this.onStarted (p : ProcessStartInfo) = ignore()


    let createServiceImpl i =
        let w = WorkerNodeRunner i
        let h = new EventHandler(EventHandlerInfo.defaultValue w.getMessages)
        do h.start()
        w


    type WorkerNodeService () =
        inherit MarshalByRefObject()

        let w =
            match MsgResponseHandler.tryCreate serviceAccessInfo.workNodeMsgAccessInfo.messagingClientAccessInfo with
            | Some h ->
                {
                    workerNodeAccessInfo = serviceAccessInfo
                    msgResponseHandler = h
                    msgClientProxy = MessagingClientProxy.defaultValue
                    workerNodeProxy = WorkerNodeProxy WorkerNodeProxyInfo.defaultValue
                    logger = logger
                    exeName = SolverRunnerName
                    minUsefulEe = MinUsefulEe.defaultValue

                }
                |> WorkerNodeRunner
                |> Some
            | None -> None

        let initService () = ()
        do initService ()

        let updateLocalProgressImpl p =
            match w with
            | Some r -> r.updateProgress p
            | None -> logger.logErr (sprintf "Failed to update progress: %A" p)

        interface IWorkerNodeService with
            member __.updateLocalProgress p = updateLocalProgressImpl p
