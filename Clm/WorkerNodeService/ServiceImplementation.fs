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


    type WorkerNodeMessage =
        | Start
        | Register
        | UpdateProgress of ProgressUpdateInfo
        | SaveModelData of ModelData
        | SaveCharts of ChartInfo
        | GetMessages of WorkerNodeRunner
        | ProcessMessage of Message


    and WorkerNodeRunner(i : WorkerNodeRunnerData) =
        let d =
            {
                msgAccessInfo = i.workerNodeAccessInfo.msgCliAccessInfo
                msgResponseHandler = i.msgResponseHandler
                msgClientProxy = i.msgClientProxy
                logger = logger
            }

        let messagingClient = MessagingClient d
        let partitioner = i.workerNodeAccessInfo.prtMsgClientId
        let storage = failwith ""
        let sendMessage m = messagingClient.sendMessage m

        let onStart s =
            s


        let onRegister s =
            {
                recipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = i.workerNodeAccessInfo.workerNodeInfo |> RegisterWorkerNodeMsg |> WorkerNodeOutMsg
            }
            |> sendMessage

            s


        let onUpdateProgress s (p : ProgressUpdateInfo) =
            let notifyPartitioner() =
                {
                    recipient = partitioner
                    deliveryType = GuaranteedDelivery
                    messageData = RunCompleted |> WorkerNodeOutMsg
                }
                |> sendMessage

            let notifyStorage t =
                {
                    recipient = storage
                    deliveryType = t
                    messageData = p |> UpdateProgressMsg |> WorkerNodeOutMsg
                }
                |> sendMessage

            match p.progress with
            | NotStarted -> notifyStorage NonGuaranteedDelivery
            | InProgress _ -> notifyStorage NonGuaranteedDelivery
            | Completed ->
                notifyPartitioner()
                notifyStorage GuaranteedDelivery

            s


        let onSaveModelData s x =
            {
                recipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = x |> SaveModelDataMsg |> WorkerNodeOutMsg
            }
            |> sendMessage

            s


        let onSaveCharts s c =
            {
                recipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = c |> SaveChartsMsg |> WorkerNodeOutMsg
            }
            |> sendMessage

            s


        let onGetMessages s (w : WorkerNodeRunner) =
            let messages = messagingClient.getMessages()

            messages
            |> List.filter (fun e -> match e.messageInfo.deliveryType with | GuaranteedDelivery -> true | NonGuaranteedDelivery -> false)
            |> List.map (fun e -> i.msgClientProxy.saveMessage IncomingMessage e)
            |> ignore

            messages
            |> List.map (fun e -> w.processMessage e)
            |> ignore

            s


        let onRunModelMsg (m : ModelData) =
            failwith ""


        let onProcessMessage s (m : Message) =
            match m.messageInfo.messageData with
            | WorkerNodeInMsg x ->
                match x with
                | RunModelMsg m -> onRunModelMsg m
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
                            | GetMessages w -> return! onGetMessages s w |> loop
                            | ProcessMessage m -> return! onProcessMessage s m |> loop
                        }

                onStart (WorkerNodeRunnerState.defaultValue) |> loop
                )

        member __.start() = Start |> messageLoop.Post
        member __.register() = Register |> messageLoop.Post
        member __.updateProgress p = UpdateProgress p |> messageLoop.Post
        member __.saveModelData m = SaveModelData m |> messageLoop.Post
        member __.saveCharts c = SaveCharts c |> messageLoop.Post
        member this.getMessages() = GetMessages this |> messageLoop.Post
        member private __.processMessage m = ProcessMessage m |> messageLoop.Post


    let createServiceImpl i =
        let w = WorkerNodeRunner i
        let h = new EventHandler(EventHandlerInfo.defaultValue w.getMessages)
        do h.start()
        w


    type WorkerNodeService () =
        inherit MarshalByRefObject()

        let w =
            match MsgResponseHandler.tryCreate serviceAccessInfo.msgCliAccessInfo with
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
