namespace ContGenServiceInfo

open System
open System.ServiceModel
open System.Threading
open ClmSys.MessagingPrimitives
open ClmSys.PartitionerPrimitives
open FSharp.Configuration

open ClmSys.GeneralData
open Clm.ModelParams
open ClmSys.GeneralPrimitives
open ClmSys.SolverRunnerPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.ContGenPrimitives
open ClmSys.ClmErrors
open ClmSys.ContGenErrors
open ClmSys.ContGenData
open ClmSys.PartitionerData

module ServiceInfo =

    let contGenServiceProgramName = "ContGenService.exe"


    [<Literal>]
    let ContGenWcfServiceName = "ContGenWcfService"


    type RunningProcessData =
        {
            modelDataId : ModelDataId
            defaultValueId : ClmDefaultValueId
            runQueueId : RunQueueId
            workerNodeId : WorkerNodeId
            commandLineParams : ModelCommandLineParam
        }


    type ProgressUpdateInfo =
        {
            runQueueId : RunQueueId
            progress : TaskProgress
        }


    type RunningProcessInfo =
        {
            started : DateTime
            progressUpdateInfo : ProgressUpdateInfo
        }

        //override r.ToString() =
        //    let (ModelDataId modelDataId) = r.progressUpdateInfo.processStartedInfo.runningProcessData.modelDataId
        //    let s = (DateTime.Now - r.started).ToString("d\.hh\:mm")
        //
        //    let estCompl =
        //        match r.progressUpdateInfo.progress.estimateEndTime r.started with
        //        | Some e -> " ETC: " + e.ToString("yyyy-MM-dd.HH:mm") + ";"
        //        | None -> EmptyString
        //
        //    sprintf "{ T: %s;%s DF: %s; MDID: %A; PID: %s; %A }"
        //        s estCompl (r.progressUpdateInfo.processStartedInfo.runningProcessData.defaultValueId.ToString()) modelDataId (r.progressUpdateInfo.processStartedInfo.processId.ToString()) r.progressUpdateInfo.progress


    type ProgressUpdateInfo
        with
        member this.toRunningProcessInfo() =
            {
                started = DateTime.Now
                progressUpdateInfo = this
            }


    let mutable private callCount = -1


    let getServiceState (getState : unit -> (list<RunQueue> * UnitResult)) =
        if Interlocked.Increment(&callCount) = 0
        then
            try
                printfn "Getting state at %s ..." (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss"))
                let (q, e) = getState()
                let r0 = q |> List.sortBy (fun e -> e.progress) |> List.map (fun e -> "      " + e.ToString()) |> String.concat Nl
                let r = if r0 = EmptyString then "[]" else Nl + "    [" + Nl + r0 + Nl + "    ]"
                printfn "... state at %s\n{\n  running = %s\n  runningCount = %A\n }"  (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss")) r q.Length
            with
            | e -> printfn "Exception occurred: %A" e
        else
            printfn "Not getting state at %A because callCount = %A." DateTime.Now callCount
            ignore()

        Interlocked.Decrement(&callCount) |> ignore
        Ok()


    type IContGenService =
        abstract tryCancelRunQueue : RunQueueId -> CancellationType -> UnitResult
        abstract tryRequestResults : RunQueueId -> ResultNotificationType -> UnitResult


    /// https://gist.github.com/dgfitch/661656
    [<ServiceContract(ConfigurationName = ContGenWcfServiceName)>]
    type IContGenWcfService =

        [<OperationContract(Name = "tryCancelRunQueue")>]
        abstract tryCancelRunQueue : q:byte[] -> byte[]


        [<OperationContract(Name = "tryRequestResults")>]
        abstract tryRequestResults : q:byte[] -> byte[]


    [<Literal>]
    let ContGenAppConfigFile = __SOURCE_DIRECTORY__ + @"\..\ContGenService\app.config"


    type ContGenAppSettings = AppSettings<ContGenAppConfigFile>


    type ContGenSettings
        with
        member w.trySaveSettings() =
            match w.isValid() with
            | Ok() ->
                try
                    ContGenAppSettings.ContGenSvcAddress <- w.contGenSvcAddress.value.value
                    ContGenAppSettings.ContGenSvcPort <- w.contGenSvcPort.value.value
                    ContGenAppSettings.MinUsefulEe <- w.minUsefulEe.value
                    ContGenAppSettings.MsgSvcAddress <- w.msgSvcAddress.value.value
                    ContGenAppSettings.MsgSvcPort <- w.msgSvcPort.value.value
                    ContGenAppSettings.PartitionerId <- w.partitionerId.value.value

                    Ok()
                with
                | e -> e |> ContGenSettingExn |> ContGenSettingsErr |> ContGenServiceErr |> Error
            | Error e -> Error e


    let loadContGenSettings() =
        let w =
            {
                contGenSvcAddress =
                    match ContGenAppSettings.ContGenSvcAddress with
                    | EmptyString -> ContGenServiceAddress.defaultValue
                    | s -> s |> ServiceAddress |> ContGenServiceAddress
                contGenSvcPort =
                    match ContGenAppSettings.ContGenSvcPort with
                    | n when n > 0 -> n |> ServicePort |> ContGenServicePort
                    | _ -> ContGenServicePort.defaultValue
                minUsefulEe = ContGenAppSettings.MinUsefulEe |> MinUsefulEe
                msgSvcAddress =
                    match ContGenAppSettings.MsgSvcAddress with
                    | EmptyString -> MessagingServiceAddress.defaultValue
                    | s -> s |> ServiceAddress |> MessagingServiceAddress
                msgSvcPort =
                    match ContGenAppSettings.MsgSvcPort with
                    | n  when n > 0 -> n |> ServicePort |> MessagingServicePort
                    | _ -> MessagingServicePort.defaultValue
                partitionerId =
                    match ContGenAppSettings.PartitionerId with
                    | p when p <> Guid.Empty -> p |> MessagingClientId |> PartitionerId
                    | _ -> defaultPartitionerId
            }
        w


    let saveContGenSettings loadSettings tryGetSaveSettings =
        let (w : ContGenSettings) = loadSettings()

        let r =
            match tryGetSaveSettings() with
            | Some() -> w.trySaveSettings()
            | None -> Ok()

        match r with
        | Ok() -> printfn "Successfully saved settings."
        | Error e -> printfn "Error occurred trying to save settings: %A." e
