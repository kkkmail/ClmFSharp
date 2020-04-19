namespace ContGenService

open Argu
open ClmSys.ServiceInstaller
open ClmSys.Logging
open ContGenService.SvcCommandLine
open ContGenService.WindowsService
open ContGenServiceInfo.ServiceInfo
open ClmSys.ContGenPrimitives

module ContGenServiceTasks =

    //type ContGenConfigParam
    //    with
    //    static member fromParseResults (p : ParseResults<ContGenRunArgs>) =
    //        [
    //            p.TryGetResult MinimumUsefulEe |> Option.bind (fun ee -> ee |> SetMinUsefulEe |> Some)
    //        ]
    //        |> List.choose id


    //let runService l ((p : list<unit>), i) = startServiceRun l i |> Some
    let runService l i = startWcfServiceRun l i
    let cleanupService logger i = ignore()


    let serviceInfo =
        {
            serviceName = contGenServiceName.value
            runService = runService
            cleanup = cleanupService
            timeoutMilliseconds = None
            logger = logger
        }


    //let getParams p = ContGenConfigParam.fromParseResults p, p.GetAllResults()
    //let getParams (p : ParseResults<ContGenRunArgs>) = [], p.GetAllResults()
    //let getSaveSettings (p : ParseResults<ContGenRunArgs>) () = p.GetAllResults() |> saveSettings
    //type ContGenServiceTask = ServiceTask<ContGenWindowsService, (list<unit> * list<ContGenRunArgs>), ContGenRunArgs>

    let getParams logger (p : ParseResults<ContGenRunArgs>) = getContGenServiceData logger (p.GetAllResults())
    let getSaveSettings (p : ParseResults<ContGenRunArgs>) () = p.GetAllResults() |> saveSettings
    type ContGenServiceTask = ServiceTask<ContGenWindowsService, ContGenServiceData, ContGenRunArgs>
