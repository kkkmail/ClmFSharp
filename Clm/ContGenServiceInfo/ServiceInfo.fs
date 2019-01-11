namespace ContGenServiceInfo
open System

module ServiceInfo =

    [<Literal>]
    let ContGenServiceName = "ContGenService"

    [<Literal>]
    let ContGenServicePort = 12346

    [<Literal>]
    let ProgramName = "ContGenService.exe"


    type TaskProgress =
        | NotStarted
        | InProgress of decimal
        | Completed

        static member create d =
            match d with 
            | _ when d <= 0.0m -> NotStarted
            | _ when d < 1.0m -> InProgress d
            | _ -> Completed


    type ProgressUpdateInfo =
        {
            updatedProcessId : int
            progress : TaskProgress
        }


    type RunningProcessInfo =
        {
            started : DateTime
            runningProcessId : int
            runningModelId : int64
            progress : TaskProgress
        }


    type ContGenRunnerState =
        {
            generating : bool
            runningCount : int
            running : RunningProcessInfo[]
            queue : int64[]
            shuttingDown : bool
        }


    type IContGenService =
        abstract getState : unit -> ContGenRunnerState
        abstract startGenerate : unit -> unit
        abstract updateProgress : ProgressUpdateInfo -> unit
        abstract requestShutDown : unit -> unit
        abstract stopGenerate : unit -> unit

