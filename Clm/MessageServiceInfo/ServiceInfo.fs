namespace MessageServiceInfo

open System
open System.Diagnostics
open ClmSys.GeneralData
open System.Threading

module ServiceInfo =

    [<Literal>]
    let ServiceName = "MessageService"

    [<Literal>]
    let ProgramName = "MessageService.exe"


    let getServiceUrl (i : ServiceAccessInfo) =
        getServiceUrlImpl i.serviceAddress.value i.servicePort.value ServiceName


    //type ContGenConfigParam =
    //    | SetToIdle
    //    | SetToCanGenerate
    //    | RequestShutDown of waitForCompletion : bool
    //    | SetRunLimit of numberOfCores : int
    //    | CancelTask of processId : int
    //    | SetMinUsefulEe of ee : double


    //type IMessageService =
    //    abstract getState : unit -> ContGenRunnerState
    //    abstract loadQueue : unit -> unit
    //    abstract startGenerate : unit -> unit
    //    abstract updateProgress : ProgressUpdateInfo -> unit
    //    abstract configureService : ContGenConfigParam -> unit
    //    abstract runModel : ModelDataId -> ModelCommandLineParam -> unit
