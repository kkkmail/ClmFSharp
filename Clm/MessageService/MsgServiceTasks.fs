namespace MessagingService

open Argu
open ClmSys.ServiceInstaller
open ClmSys.MessagingData
open ClmSys.Logging
open ClmSys.MessagingPrimitives
open MessagingService.SvcCommandLine
open MessagingService.WindowsService
open MessagingServiceInfo.ServiceInfo

module ServiceTasks =

    type MessagingConfigParam
        with
        static member fromParseResults (p : ParseResults<MessagingServiceRunArgs>) : list<MessagingConfigParam> =
            [
            ]
            |> List.choose id


    let runService l (_ : list<MessagingConfigParam>, i) = startWcfServiceRun l i


    let cleanupService (logger : Logger) i =
        logger.logInfoString "MessagingWindowsService: Unregistering TCP channel."
        //ChannelServices.UnregisterChannel(i.msgSvcTcpChannel)
        printfn "cleanupService is not yet implemented."


    let serviceInfo =
        {
            serviceName = messagingServiceName.value
            runService = runService
            cleanup = cleanupService
            timeoutMilliseconds = None
            logger = logger
        }


    let getParams p = MessagingConfigParam.fromParseResults p, getServiceAccessInfo (p.GetAllResults())
    let getSaveSettings (p : ParseResults<MessagingServiceRunArgs>) () = p.GetAllResults() |> saveSettings
    type MessagingServiceTask = ServiceTask<MessagingWindowsService, (list<MessagingConfigParam> * MessagingServiceAccessInfo), MessagingServiceRunArgs>
