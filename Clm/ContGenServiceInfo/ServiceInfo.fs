namespace ContGenServiceInfo
open System
open ClmSys.GeneralData

module ServiceInfo =

    [<Literal>]
    let ContGenServiceAddress = "localhost"

    [<Literal>]
    let ContGenServiceName = "ContGenService"

    [<Literal>]
    let ContGenServicePort = 12345

    [<Literal>]
    let ProgramName = "ContGenService.exe"


    let getServiceUrl() = "tcp://" + ContGenServiceAddress + ":" + (ContGenServicePort.ToString()) + "/" + ContGenServiceName


    type TaskProgress =
        | NotStarted
        | InProgress of decimal
        | Completed

        static member create d =
            match d with 
            | _ when d <= 0.0m -> NotStarted
            | _ when d < 1.0m -> InProgress d
            | _ -> Completed


    type WorkState =
        | Idle
        | CanGenerate
        | ShuttingDown


    type ProgressUpdateInfo =
        {
            updatedProcessId : int
            updateModelId : ModelDataId
            progress : TaskProgress
        }


    type RunningProcessInfo =
        {
            started : DateTime
            runningProcessId : int
            runningModelId : ModelDataId
            progress : TaskProgress
        }


    type ContGenRunnerState =
        {
            //generating : bool
            runLimit : int
            maxQueueLength : int
            runningCount : int
            running : RunningProcessInfo[]
            queue : ModelDataId[]
            workState : WorkState
        }


    type ContGenConfigParam =
        | SetToIdle
        | SetToCanGenerate
        | RequestShutDown of waitForCompletion : bool
        | SetRunLimit of int
        | CancelTask of processId : int


    type IContGenService =
        abstract getState : unit -> ContGenRunnerState
        abstract startGenerate : unit -> unit
        abstract updateProgress : ProgressUpdateInfo -> unit
        abstract configureService : ContGenConfigParam -> unit
