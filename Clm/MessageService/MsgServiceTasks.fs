namespace MessagingService

open Argu

open ClmSys.ServiceInstaller
open ClmSys.MessagingData
open ClmSys.Logging
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


    let runService l (_, i) = startServiceRun l i


    let serviceInfo =
        {
            serviceName = ServiceName MessagingServiceName
            runService = runService
            timeoutMilliseconds = None
            logger = logger
        }


    let getParams p = MessagingConfigParam.fromParseResults p, getServiceAccessInfo (p.GetAllResults())
    type MessagingServiceTask = ServiceTask<MessagingWindowsService, (list<MessagingConfigParam> * MessagingServiceAccessInfo), MessagingServiceRunArgs>
