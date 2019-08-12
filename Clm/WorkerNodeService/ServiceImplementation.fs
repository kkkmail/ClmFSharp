namespace WorkerNodeService

open System
open ClmSys.GeneralData
open ClmSys.Logging
open ClmSys.MessagingData
open ClmSys.WorkerNodeData
open ClmSys.TimerEvents
open ContGenServiceInfo.ServiceInfo
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.SvcCommandLine
open MessagingServiceInfo.ServiceProxy
open MessagingServiceInfo.ServiceInfo
open Messaging.Client
open Messaging.ServiceResponse
open Argu
open Clm.CalculationData

module ServiceImplementation =

    let mutable serviceAccessInfo =
        let parser = ArgumentParser.Create<WorkerNodeServiceRunArgs>(programName = WorkerNodeServiceProgramName)
        let results = (parser.Parse [||]).GetAllResults()
        results |> getServiceAccessInfo


    type WorkerNodeMessage =
        | Start
        | Register
        | UpdateProgress of ProgressUpdateInfo
        | SaveModelData of ModelData
        | SaveCharts of ChartInfo
        | GetMessages


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
            messagingClientProxy : MessagingClientProxy
            logger : Logger
        }


    type WorkerNodeRunner(i : WorkerNodeRunnerData) =
        let d =
            {
                msgAccessInfo = i.workerNodeAccessInfo.msgCliAccessInfo
                msgResponseHandler = i.msgResponseHandler
                msgClientProxy = i.messagingClientProxy
                logger = logger
            }

        let messagingClient = MessagingClient d
        let partitioner = i.workerNodeAccessInfo.prtMsgClientId
        let sendMessage m = messagingClient.sendMessage m

        let onStart s =
            s


        let onRegister s =
            {
                recipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = i.workerNodeAccessInfo.workerNodeInfo |> RegisterWorkerNodeMsg |> WorkerNodeMsg
            }
            |> sendMessage

            s


        let onUpdateProgress s (p : ProgressUpdateInfo) =
            {
                recipient = partitioner
                deliveryType =
                    match p.progress with
                    | NotStarted -> NonGuaranteedDelivery
                    | InProgress _ -> NonGuaranteedDelivery
                    | Completed -> GuaranteedDelivery
                messageData = p |> UpdateProgressMsg |> WorkerNodeMsg
            }
            |> sendMessage

            s


        let onSaveModelData s x =
            {
                recipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = x |> SaveModelDataMsg |> WorkerNodeMsg
            }
            |> sendMessage

            s


        let onSaveCharts s c =
            {
                recipient = partitioner
                deliveryType = GuaranteedDelivery
                messageData = c |> SaveChartsMsg |> WorkerNodeMsg
            }
            |> sendMessage

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
                            | GetMessages -> return! s |> loop
                        }

                onStart (WorkerNodeRunnerState.defaultValue) |> loop
                )

        member this.start() = Start |> messageLoop.Post
        member this.register() = Register |> messageLoop.Post
        member this.updateProgress p = UpdateProgress p |> messageLoop.Post
        member this.saveModelData m = SaveModelData m |> messageLoop.Post
        member this.saveCharts c = SaveCharts c |> messageLoop.Post
        member this.getMessages() = GetMessages |> messageLoop.Post


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
                    messagingClientProxy = MessagingClientProxy.defaultValue
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
