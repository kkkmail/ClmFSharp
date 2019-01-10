namespace ContGenServiceInfo

module ServiceInfo =

    [<Literal>]
    let ContGenServiceName = "ContGenService"

    [<Literal>]
    let ContGenServicePort = "12346"

    [<Literal>]
    let ProgramName = "ContGenService.exe"


    type ContGenRunnerState =
        {
            generating : bool
            runningCount : int
            //running : Map<int, RunningProcessInfo>
            //queue : list<RunInfo>
            //shuttingDown : bool
        }


    type IContGenService =
        abstract getState : unit -> ContGenRunnerState
        abstract startGenerating : unit -> unit
        abstract stopGenerating : unit -> unit
