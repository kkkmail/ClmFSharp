namespace ContGenService

open Argu
open ClmSys.GeneralData
open ClmSys.ServiceInstaller
open ClmSys.Logging
open ContGenService.SvcCommandLine
open ContGenService.WindowsService
open ContGenServiceInfo.ServiceInfo
open System.Runtime.Remoting.Channels
open ClmSys.ContGenData

module ContGenServiceTasks =

    type ContGenConfigParam
        with
        static member fromParseResults (p : ParseResults<ContGenRunArgs>) =
            [
                p.TryGetResult NumberOfCores |> Option.bind (fun c -> SetRunLimit c |> Some)
                p.TryGetResult RunIdle |> Option.bind (fun _ -> Some SetToIdle)
                p.TryGetResult MinimumUsefulEe |> Option.bind (fun ee -> ee |> SetMinUsefulEe |> Some)
            ]
            |> List.choose id


    let runService l (p, i) : ContGenShutDownInfo option =
        let s = startServiceRun l i

        match s with
        | Some svc ->
            let service = svc.service
            p |> List.map (fun e -> service.configureService e) |> ignore
        | None -> ignore()
        s


    let cleanupService (logger : Logger) i =
        logger.logInfoString "ContGenWindowsService: Unregistering TCP channel."
        ChannelServices.UnregisterChannel(i.contGenTcpChannel)


    let serviceInfo =
        {
            serviceName = ServiceName ContGenServiceName
            runService = runService
            cleanup = cleanupService
            timeoutMilliseconds = None
            logger = logger
        }


    let getParams p = ContGenConfigParam.fromParseResults p, getServiceAccessInfo (p.GetAllResults())
    let getSaveSettings (p : ParseResults<ContGenRunArgs>) () = p.GetAllResults() |> saveSettings
    type ContGenServiceTask = ServiceTask<ContGenWindowsService, (list<ContGenConfigParam> * ContGenServiceAccessInfo), ContGenRunArgs>
