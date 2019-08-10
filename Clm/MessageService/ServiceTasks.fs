namespace MessagingService

open System
open System.Threading
open System.Configuration.Install
open System.ServiceProcess
open Argu

open ClmSys.VersionInfo
open ClmSys.GeneralData
open ClmSys.ServiceInstaller
open ClmSys.MessagingData
open MessagingService.SvcCommandLine
open MessagingService.WindowsService
open MessagingServiceInfo.ServiceInfo

module ServiceTasks =

    type MessagingConfigParam
        with
        static member fromParseResults (p : ParseResults<MessagingServiceRunArgs>) : list<MessagingConfigParam> =
            [
                //p.TryGetResult NumberOfCores |> Option.bind (fun c -> SetRunLimit c |> Some)
                //p.TryGetResult RunIdle |> Option.bind (fun _ -> Some SetToIdle)
                //p.TryGetResult MinimumUsefulEe |> Option.bind (fun ee -> ee |> SetMinUsefulEe |> Some)
            ]
            |> List.choose id


    /// TODO kk:20190520 - Propagate p into service.
    let startMsgService timeoutMilliseconds (p : list<MessagingConfigParam>) =
        (startService MessagingServiceName timeoutMilliseconds)


    let stopMsgService timeoutMilliseconds = (stopService MessagingServiceName timeoutMilliseconds)


    let runService i (p : list<MessagingConfigParam>) =
        printfn "Starting..."
        let waitHandle = new ManualResetEvent(false)
        let logger e = printfn "Error: %A" e
        startServiceRun i logger
        waitHandle.WaitOne() |> ignore
        true


    /// TODO kk:20190601 - Propagage address / port into the service installation call.
    type MessagingServiceTask =
        | InstallServiceTask
        | UninstallServiceTask
        | StartServiceTask of list<MessagingConfigParam>
        | StopServiceTask
        | RunServiceTask of list<MessagingConfigParam> * MessagingServiceAccessInfo

        member task.run() =
            match task with
            | InstallServiceTask -> installService<MessagingWindowsService> MessagingServiceName
            | UninstallServiceTask ->
                match stopMsgService ServiceTmeOut with
                | true -> printfn "Successfully stopped service."
                | false -> printfn "Failed to stop service! Proceeding with uninstall anyway."

                uninstallService<MessagingWindowsService> MessagingServiceName
            | StartServiceTask p -> startMsgService ServiceTmeOut p
            | StopServiceTask -> stopMsgService ServiceTmeOut
            | RunServiceTask (p, i) -> runService i p

        static member private tryCreateInstallServiceTask (p : list<MsgSvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Install -> InstallServiceTask |> Some | _ -> None)

        static member private tryCreateUninstallServiceTask (p : list<MsgSvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Uninstall -> UninstallServiceTask |> Some | _ -> None)

        static member private tryCreateStartServiceTask (p : list<MsgSvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Start p -> MessagingConfigParam.fromParseResults p |> StartServiceTask |> Some | _ -> None)

        static member private tryCreateStopServiceTask (p : list<MsgSvcArguments>) =
            p |> List.tryPick (fun e -> match e with | Stop -> StopServiceTask |> Some | _ -> None)

        static member private tryCreateRunServiceTask (p : list<MsgSvcArguments>) =
            p |> List.tryPick (fun e ->
                                match e with
                                | Run p ->
                                    let i = getServiceAccessInfo (p.GetAllResults())
                                    RunServiceTask(MessagingConfigParam.fromParseResults p, i) |> Some
                                | _ -> None)

        static member tryCreate (p : list<MsgSvcArguments>) =
            [
                MessagingServiceTask.tryCreateUninstallServiceTask
                MessagingServiceTask.tryCreateInstallServiceTask
                MessagingServiceTask.tryCreateStopServiceTask
                MessagingServiceTask.tryCreateStartServiceTask
                MessagingServiceTask.tryCreateRunServiceTask
            ]
            |> List.tryPick (fun e -> e p)
