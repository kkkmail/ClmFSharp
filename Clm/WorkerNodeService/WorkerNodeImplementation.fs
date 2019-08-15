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

module ServiceImplementation =

    let mutable serviceAccessInfo =
        let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = WorkerNodeServiceProgramName)
        let results = (parser.Parse [||]).GetAllResults()
        results |> getServiceAccessInfo


    type WorkerNodeRunnerState =
        {
            dummy : int
        }

        static member defaultValue =
            {
                dummy = 0
            }


    type WorkerNodeRunnerData =
        {
            workerNodeAccessInfo : WorkerNodeServiceAccessInfo
            msgResponseHandler : MsgResponseHandler
            msgClientProxy : MessagingClientProxy
            logger : Logger
        }

        member this.messagingClientData =
            {
                msgAccessInfo = this.workerNodeAccessInfo.msgCliAccessInfo.messagingClientAccessInfo
                msgResponseHandler = this.msgResponseHandler
                msgClientProxy = this.msgClientProxy
                logger = this.logger
            }


    type WorkerNodeMessage =
        | Start
        | Register
        | UpdateProgress of ProgressUpdateInfo
        | SaveModelData of ModelData
        | SaveResult of unit
        | SaveCharts of ChartInfo
        | GetMessages of WorkerNodeRunner
        | ProcessMessage of WorkerNodeRunner * Message
        | RunModel of WorkerNodeRunner * ModelData


    and WorkerNodeRunner(i : WorkerNodeRunnerData) =
        let messagingClient = MessagingClient i.messagingClientData
        let partitioner = i.workerNodeAccessInfo.partitionerId
        let sendMessage m = messagingClient.sendMessage m

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


        let onUpdateProgress s (p : ProgressUpdateInfo) =
            let notifyPartitioner t =
                {
                    partitionerRecipient = partitioner
                    deliveryType = t
                    messageData = UpdateProgressPrtMsg p
                }.messageInfo
                |> sendMessage

            match p.progress with
            | NotStarted -> NonGuaranteedDelivery
            | InProgress _ -> NonGuaranteedDelivery
            | Completed -> GuaranteedDelivery
            |> notifyPartitioner

            s


        let onSaveModelData s x =
            {
                partitionerRecipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = x |> SaveModelDataPrtMsg
            }.messageInfo
            |> sendMessage

            s


        let onSaveResult s r =
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


        let onRunModel s (r : WorkerNodeRunner) (m : ModelData) =
            s


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
                            | SaveModelData m -> return! onSaveModelData s m |> loop
                            | SaveCharts c -> return! onSaveCharts s c |> loop
                            | GetMessages r -> return! onGetMessages s r |> loop
                            | ProcessMessage (r, m) -> return! onProcessMessage s r m |> loop
                            | RunModel (r, m) -> return! onRunModel s r m |> loop
                        }

                onStart (WorkerNodeRunnerState.defaultValue) |> loop
                )

        member __.start() = Start |> messageLoop.Post
        member __.register() = Register |> messageLoop.Post
        member __.updateProgress p = UpdateProgress p |> messageLoop.Post
        member __.saveModelData m = SaveModelData m |> messageLoop.Post
        member __.saveCharts c = SaveCharts c |> messageLoop.Post
        member this.getMessages() = GetMessages this |> messageLoop.Post
        member private this.processMessage m = ProcessMessage (this, m) |> messageLoop.Post
        member private this.runModel m = RunModel (this, m) |> messageLoop.Post


    let createServiceImpl i =
        let w = WorkerNodeRunner i
        let h = new EventHandler(EventHandlerInfo.defaultValue w.getMessages)
        do h.start()
        w


    type WorkerNodeService () =
        inherit MarshalByRefObject()

        let w =
            match MsgResponseHandler.tryCreate serviceAccessInfo.msgCliAccessInfo.messagingClientAccessInfo with
            | Some h ->
                {
                    workerNodeAccessInfo = serviceAccessInfo
                    msgResponseHandler = h
                    msgClientProxy = MessagingClientProxy.defaultValue
                    logger = logger
                }
                |> WorkerNodeRunner
                |> Some
            | None -> None

        let initService () = ()
        do initService ()

        let updateProgressImpl p =
            match w with
            | Some r -> r.updateProgress p
            | None -> logger.logErr (sprintf "Failed to update progress: %A" p)

        interface IWorkerNodeService with
            member __.updateProgress p = updateProgressImpl p
