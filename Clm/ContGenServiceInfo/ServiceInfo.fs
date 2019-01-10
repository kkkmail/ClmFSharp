namespace ContGenServiceInfo

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


    type ContGenRunnerState =
        {
            generating : bool
            runningCount : int
            //running : Map<int, RunningProcessInfo>
            //queue : list<RunInfo>
            //shuttingDown : bool
        }


    type IProgressNotifier =
        abstract notifyOfProgress : ProgressUpdateInfo -> unit

    type IContGenService =
        inherit IProgressNotifier
        abstract getState : unit -> ContGenRunnerState
        abstract startGenerating : unit -> unit
        abstract stopGenerating : unit -> unit
