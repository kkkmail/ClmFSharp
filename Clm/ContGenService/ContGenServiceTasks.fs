﻿namespace ContGenService

open Argu
open ClmSys.ServiceInstaller
open ClmSys.Logging
open ContGenService.SvcCommandLine
open ContGenService.WindowsService
open ContGenServiceInfo.ServiceInfo
open System.Runtime.Remoting.Channels
open ClmSys.ContGenData
open ClmSys.ContGenPrimitives

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


    let runService l ((p : list<ContGenConfigParam>), i) : ContGenShutDownInfo option =
        let s = startServiceRun l i
        s


    let cleanupService (logger : Logger) i =
        logger.logInfoString "ContGenWindowsService: Unregistering TCP channel."
        ChannelServices.UnregisterChannel(i.contGenTcpChannel)


    let serviceInfo =
        {
            serviceName = contGenServiceName.value
            runService = runService
            cleanup = cleanupService
            timeoutMilliseconds = None
            logger = logger
        }


    let getParams p = ContGenConfigParam.fromParseResults p, getServiceAccessInfo (p.GetAllResults())
    let getSaveSettings (p : ParseResults<ContGenRunArgs>) () = p.GetAllResults() |> saveSettings
    type ContGenServiceTask = ServiceTask<ContGenWindowsService, (list<ContGenConfigParam> * ContGenServiceAccessInfo), ContGenRunArgs>
