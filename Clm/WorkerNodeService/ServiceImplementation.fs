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

        let onStart s =
            s


        let onRegister s =
            
            s


        let onUpdateProgress s p =
            s


        let onSaveModelData s m =
            s


        let onSaveCharts s c =
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
                        }

                onStart (WorkerNodeRunnerState.defaultValue) |> loop
                )

        member this.start() = Start |> messageLoop.Post
        member this.register() = Register |> messageLoop.Post
        member this.updateProgress p = UpdateProgress p |> messageLoop.Post
        member this.saveModelData m = SaveModelData m |> messageLoop.Post
        member this.saveCharts c = SaveCharts c |> messageLoop.Post


    type WorkerNodeServiceImpl(i : WorkerNodeServiceAccessInfo) =

        member this.onTimer() = ignore()
        member this.updateProgress (p: ProgressUpdateInfo) : unit = failwith ""


    let createServiceImpl i =
        let w = WorkerNodeServiceImpl i
        let h = new EventHandler(EventHandlerInfo.defaultValue w.onTimer)
        do h.start()
        w


    type WorkerNodeService () =
        inherit MarshalByRefObject()

        let w = createServiceImpl serviceAccessInfo
        let initService () = ()
        do initService ()

        let notSupported a =
            let msg = (sprintf "The method %A is not supported by WorkerNodeService." a)
            printfn "%s" msg
            failwith msg

        interface IContGenService with
            member __.getState() = notSupported "getState"
            member __.loadQueue() = notSupported "loadQueue"
            member __.startGenerate() = notSupported "startGenerate"
            member __.updateProgress p = w.updateProgress p
            member __.configureService _ = notSupported "configureService"
            member __.runModel _ _ = notSupported "runModel"

