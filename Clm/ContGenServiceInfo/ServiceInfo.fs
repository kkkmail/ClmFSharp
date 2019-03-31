namespace ContGenServiceInfo
open System
open ClmSys.GeneralData

module ServiceInfo =

    [<Literal>]
    let ContGenServiceName = "ContGenService"

    [<Literal>]
    let ProgramName = "ContGenService.exe"


    let getServiceUrlImpl contGenServiceAddress (contGenServicePort : int) contGenServiceName =
        "tcp://" + contGenServiceAddress + ":" + (contGenServicePort.ToString()) + "/" + contGenServiceName


    let getServiceUrl() = getServiceUrlImpl ContGenServiceAddress ContGenServicePort ContGenServiceName


    type TaskProgress =
        | NotStarted
        | InProgress of decimal
        | Completed

        static member create d =
            match d with 
            | _ when d <= 0.0m -> NotStarted
            | _ when d < 1.0m -> InProgress d
            | _ -> Completed

        member progress.estimateEndTime (started : DateTime) =
            match progress with
            | NotStarted -> None
            | InProgress p -> estimateEndTime p started
            | Completed -> Some DateTime.Now


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

        override r.ToString() =
            let (ModelDataId modelDataId) = r.runningModelId
            let s = formatTimeSpan (DateTime.Now - r.started)

            let estCompl =
                match r.progress.estimateEndTime r.started with
                | Some e -> " est. compl.: " + e.ToShortDateString() + ", " + e.ToShortTimeString() + ";"
                | None -> EmptyString

            sprintf "{ running = %s;%s modelDataId = %A; processId = %A; progress = %A }" s estCompl modelDataId r.runningProcessId r.progress


    type ContGenRunnerState =
        {
            runLimit : int
            maxQueueLength : int
            running : RunningProcessInfo[]
            queue : ModelDataId[]
            runningCount : int
            workState : WorkState
        }

        override s.ToString() =
            let q0 = s.queue |> Array.map (fun e -> e.value.ToString()) |> String.concat "; "
            let q = if q0 = EmptyString then "[]" else "[ " + q0 + " ]"

            let r0 = s.running |> Array.map (fun e -> "            " + e.ToString()) |> String.concat Nl
            let r = if r0 = EmptyString then "[]" else Nl + "        [" + Nl + r0 + Nl + "        ]"
            sprintf "{\n    running = %s\n    queue = %s\n    runLimit = %A; runningCount = %A; workState = %A\n}" r q s.runLimit s.runningCount s.workState


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


    let getServiceState (service : IContGenService) =
        printfn "Getting state..."
        let state = service.getState()
        printfn "...state =\n%s\n\n" (state.ToString())
        if state.queue.Length = 0 then service.startGenerate()
