namespace ContGenService

open System.Threading
open Argu

open ClmSys.GeneralData
open ClmSys.ServiceInstaller
open ContGenService.SvcCommandLine
open ContGenService.WindowsService
open ContGenServiceInfo.ServiceInfo
open ContGenAdm.ContGenServiceResponse

module ContGenServiceTasks =

    type ContGenConfigParam
        with
        static member fromParseResults (p : ParseResults<ContGenRunArgs>) : list<ContGenConfigParam> =
            [
                p.TryGetResult NumberOfCores |> Option.bind (fun c -> SetRunLimit c |> Some)
                p.TryGetResult RunIdle |> Option.bind (fun _ -> Some SetToIdle)
                p.TryGetResult MinimumUsefulEe |> Option.bind (fun ee -> ee |> SetMinUsefulEe |> Some)
            ]
            |> List.choose id


    /// TODO kk:20190520 - Propagate p into service.
    let startContGenService timeoutMilliseconds (p : list<ContGenConfigParam>) =
        (startService ContGenServiceName timeoutMilliseconds)


    let stopContGenService timeoutMilliseconds =
        (stopService ContGenServiceName timeoutMilliseconds)


    let runService i (p : list<ContGenConfigParam>) =
        printfn "Starting..."
        let waitHandle = new ManualResetEvent(false)
        let logger e = printfn "Error: %A" e
        startServiceRun i logger
        let service = (new ContGenResponseHandler(i)).contGenService
        p |> List.map (fun e -> service.configureService e) |> ignore
        service.loadQueue()
        let eventHandler _ = getServiceState service

        let timer = new System.Timers.Timer(30_000.0)
        do timer.AutoReset <- true
        do timer.Elapsed.Add eventHandler
        do timer.Start()

        waitHandle.WaitOne() |> ignore
        true


    /// TODO kk:20190601 - Propagage address / port into the service installation call.
    type ContGenServiceTask = ServiceTask<ContGenWindowsService, ContGenConfigParam, ContGenSvcArguments>

        //| InstallServiceTask
        //| UninstallServiceTask
        //| StartServiceTask of list<ContGenConfigParam>
        //| StopServiceTask
        //| RunServiceTask of list<ContGenConfigParam> * ContGenServiceAccessInfo

        //member task.run() =
        //    match task with
        //    | InstallServiceTask -> installService<ContGenWindowsService> ContGenServiceName
        //    | UninstallServiceTask ->
        //        match stopContGenService ServiceTmeOut with
        //        | true -> printfn "Successfully stopped service."
        //        | false -> printfn "Failed to stop service! Proceeding with uninstall anyway."

        //        uninstallService<ContGenWindowsService> ContGenServiceName
        //    | StartServiceTask p -> startContGenService ServiceTmeOut p
        //    | StopServiceTask -> stopContGenService ServiceTmeOut
        //    | RunServiceTask (p, i) -> runService i p

        //static member private tryCreateInstallServiceTask (p : list<ContGenSvcArguments>) =
        //    p |> List.tryPick (fun e -> match e with | Install -> InstallServiceTask |> Some | _ -> None)

        //static member private tryCreateUninstallServiceTask (p : list<ContGenSvcArguments>) =
        //    p |> List.tryPick (fun e -> match e with | Uninstall -> UninstallServiceTask |> Some | _ -> None)

        //static member private tryCreateStartServiceTask (p : list<ContGenSvcArguments>) =
        //    p |> List.tryPick (fun e -> match e with | Start p -> ContGenConfigParam.fromParseResults p |> StartServiceTask |> Some | _ -> None)

        //static member private tryCreateStopServiceTask (p : list<ContGenSvcArguments>) =
        //    p |> List.tryPick (fun e -> match e with | Stop -> StopServiceTask |> Some | _ -> None)

        //static member private tryCreateRunServiceTask (p : list<ContGenSvcArguments>) =
        //    p |> List.tryPick (fun e ->
        //                        match e with
        //                        | Run p ->
        //                            let i = getServiceAccessInfo (p.GetAllResults())
        //                            RunServiceTask(ContGenConfigParam.fromParseResults p, i) |> Some
        //                        | _ -> None)

        //static member tryCreate (p : list<ContGenSvcArguments>) =
        //    [
        //        ContGenServiceTask.tryCreateUninstallServiceTask
        //        ContGenServiceTask.tryCreateInstallServiceTask
        //        ContGenServiceTask.tryCreateStopServiceTask
        //        ContGenServiceTask.tryCreateStartServiceTask
        //        ContGenServiceTask.tryCreateRunServiceTask
        //    ]
        //    |> List.tryPick (fun e -> e p)
