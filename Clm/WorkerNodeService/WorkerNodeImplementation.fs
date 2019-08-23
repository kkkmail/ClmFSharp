namespace WorkerNodeService

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
        | Start of WorkerNodeRunner
        | Register
        | UpdateProgress of LocalProgressUpdateInfo
        | SaveResult of ResultDataWithId
        | SaveCharts of ChartInfo
        | GetMessages of WorkerNodeRunner
        | ProcessMessage of WorkerNodeRunner * Message
        | RunModel of WorkerNodeRunModelData
        | GetState of AsyncReplyChannel<WorkerNodeRunnerState>


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


        let onStart s (r : WorkerNodeRunner) =
            printfn "WorkerNodeRunner.onStart"
            i.workerNodeProxy.loadAllWorkerNodeRunModelData()
            |> List.map (fun e -> r.runModel e)
            |> ignore

            s


        let onRegister s =
            printfn "WorkerNodeRunner.onRegister"
            {
                partitionerRecipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = i.workerNodeAccessInfo.workerNodeInfo |> RegisterWorkerNodePrtMsg
            }.messageInfo
            |> sendMessage

            s

        let onCompleted x =

            ignore()


        let onUpdateProgress s (p : LocalProgressUpdateInfo) =
            printfn "WorkerNodeRunner.onUpdateProgress: p = %A." p
            let updateProgress t c =
                match s.running.TryFind p.updatedLocalProcessId with
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
                        //onCompleted
                        i.workerNodeProxy.tryDeleteWorkerNodeRunModelData r |> ignore
                        i.workerNodeProxy.tryDeleteModelData p.updateModelId |> ignore
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
            printfn "WorkerNodeRunner.onSaveResult: r = %A." r
            {
                partitionerRecipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = r |> SaveResultPrtMsg
            }.messageInfo
            |> sendMessage

            s


        let onSaveCharts s c =
            printfn "WorkerNodeRunner.onSaveCharts: c = %A." c
            {
                partitionerRecipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = c |> SaveChartsPrtMsg
            }.messageInfo
            |> sendMessage

            s


        let onGetMessages s (r : WorkerNodeRunner) =
            printfn "WorkerNodeRunner.onGetMessages"
            printfn "WorkerNodeRunnerState: %A" s
            let messages = messagingClient.getMessages()

            //messages
            //|> List.filter (fun e -> match e.messageInfo.deliveryType with | GuaranteedDelivery -> true | NonGuaranteedDelivery -> false)
            //|> List.map (fun e -> i.msgClientProxy.saveMessage { messageType = IncomingMessage; message = e })
            //|> ignore

            messages
            |> List.map (fun e -> r.processMessage e)
            |> ignore

            s


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

            let result = i.workerNodeProxy.runModel a
            { s with running = s.running.Add(result.localProcessId, d.remoteProcessId) }


        let onProcessMessage s (w : WorkerNodeRunner) (m : Message) =
            printfn "WorkerNodeRunner.onProcessMessage: m.messageId = %A." m.messageId
            match m.messageInfo.messageData with
            | WorkerNodeMsg x ->
                match x with
                | RunModelWrkMsg (d, m) ->
                    i.workerNodeProxy.saveModelData m
                    w.runModel d
            | _ -> i.logger.logErr (sprintf "Invalid message type: %A." m.messageInfo.messageData)

            //match m.messageInfo.deliveryType with
            //| GuaranteedDelivery -> i.msgClientProxy.deleteMessage m.messageId
            //| NonGuaranteedDelivery -> ignore()

            s


        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | Start w -> return! onStart s w |> loop
                            | Register -> return! onRegister s |> loop
                            | UpdateProgress p -> return! onUpdateProgress s p |> loop
                            | SaveResult r -> return! onSaveResult s r |> loop
                            | SaveCharts c -> return! onSaveCharts s c |> loop
                            | GetMessages w -> return! onGetMessages s w |> loop
                            | ProcessMessage (w, m) -> return! onProcessMessage s w m |> loop
                            | RunModel d -> return! onRunModel s d |> loop
                            | GetState w -> w.Reply s
                        }

                WorkerNodeRunnerState.defaultValue |> loop
                )

        member this.start() = Start this |> messageLoop.Post
        member __.register() = Register |> messageLoop.Post
        member __.updateProgress p = UpdateProgress p |> messageLoop.Post
        member __.saveCharts c = SaveCharts c |> messageLoop.Post
        member this.getMessages() = GetMessages this |> messageLoop.Post
        member private this.processMessage m = ProcessMessage (this, m) |> messageLoop.Post
        member private __.runModel d = RunModel d |> messageLoop.Post
        member __.getState () = messageLoop.PostAndReply GetState


    let createServiceImpl i =
        printfn "createServiceImpl: Creating WorkerNodeRunner..."
        let w = WorkerNodeRunner i
        do w.register()
        do w.start()
        let h = new EventHandler(EventHandlerInfo.defaultValue w.getMessages)
        do h.start()
        w


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
                |> Some
            | None ->
                printfn "WorkerNodeService: Cannot create MsgResponseHandler."
                None

        let initService () = ()
        do initService ()

        let updateLocalProgressImpl p =
            match w with
            | Some r -> r.updateProgress p
            | None -> logger.logErr (sprintf "Failed to update progress: %A" p)

        interface IWorkerNodeService with
            member __.updateLocalProgress p = updateLocalProgressImpl p
            member __.ping() = ignore()
