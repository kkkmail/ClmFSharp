﻿namespace MessagingService

open Argu

open ClmSys.ServiceInstaller
open ClmSys.MessagingData
open ClmSys.Logging
open MessagingService.SvcCommandLine
open MessagingService.WindowsService
open MessagingServiceInfo.ServiceInfo
open System.Runtime.Remoting.Channels

module ServiceTasks =

    type MessagingConfigParam
        with
        static member fromParseResults (p : ParseResults<MessagingServiceRunArgs>) : list<MessagingConfigParam> =
            [
            ]
            |> List.choose id


    //let runService l (_, i) = startServiceRun l i
    let runService l (_, i) = startWcfServiceRun l i


    //let cleanupService logger i =
    //    logger.logInfo "MessagingWindowsService: Unregistering TCP channel."
    //    ChannelServices.UnregisterChannel(i.msgSvcTcpChannel)


    let cleanupService (logger : Logger) i =
        logger.logInfoString "MessagingWindowsService: Unregistering TCP channel."
        //ChannelServices.UnregisterChannel(i.msgSvcTcpChannel)
        printfn "cleanupService is not yet implemented."


    let serviceInfo =
        {
            serviceName = ServiceName MessagingServiceName
            runService = runService
            cleanup = cleanupService
            timeoutMilliseconds = None
            logger = logger
        }


    let getParams p = MessagingConfigParam.fromParseResults p, getServiceAccessInfo (p.GetAllResults())
    let getSaveSettings (p : ParseResults<MessagingServiceRunArgs>) () = p.GetAllResults() |> saveSettings
    type MessagingServiceTask = ServiceTask<MessagingWindowsService, (list<MessagingConfigParam> * MessagingServiceAccessInfo), MessagingServiceRunArgs>
