namespace ContGenServiceInfo

open System
open System.Diagnostics
open ClmSys.GeneralData
open ClmSys.MessagingData
open System.Threading
open Clm.ModelParams
open Clm.CalculationData

module ServiceInfo =

    [<Literal>]
    let ContGenServiceName = "ContGenService"

    [<Literal>]
    let ContGenServiceProgramName = "ContGenService.exe"


    let getServiceUrl (i : ServiceAccessInfo) =
        getServiceUrlImpl i.serviceAddress.value i.servicePort.value ContGenServiceName


    type TaskProgress =
        | NotStarted
        | InProgress of decimal
        | Completed

        static member create d =
            match d with
            | _ when d <= 0.0m -> NotStarted
            | _ when d < 1.0m -> InProgress d
            | _ -> InProgress 1.0m

        member progress.estimateEndTime (started : DateTime) =
            match progress with
            | NotStarted -> None
            | InProgress p -> estimateEndTime p started
            | Completed -> Some DateTime.Now


    type WorkState =
        | Idle
        | CanGenerate
        | ShuttingDown


    type LocalProcessId =
        | LocalProcessId of int

        member this.value = let (LocalProcessId v) = this in v


    type RemoteProcessId =
        | RemoteProcessId of Guid

        member this.value = let (RemoteProcessId v) = this in v


    type ProcessId =
        | LocalProcess of LocalProcessId
        | RemoteProcess of RemoteProcessId


    type ProgressUpdateInfo =
        {
            updatedProcessId : ProcessId
            updateModelId : ModelDataId
            progress : TaskProgress
        }


    type LocalProgressUpdateInfo =
        {
            updatedLocalProcessId : LocalProcessId
            updateModelId : ModelDataId
            progress : TaskProgress
        }

        member this.progressUpdateInfo =
            {
                updatedProcessId = this.updatedLocalProcessId |> LocalProcess
                updateModelId = this.updateModelId
                progress = this.progress
            }


    type RemoteProgressUpdateInfo =
        {
            updatedRemoteProcessId : RemoteProcessId
            updateModelId : ModelDataId
            progress : TaskProgress
        }

        member this.progressUpdateInfo =
            {
                updatedProcessId = this.updatedRemoteProcessId |> RemoteProcess
                updateModelId = this.updateModelId
                progress = this.progress
            }


    let fromLocalProgress (p : LocalProgressUpdateInfo) r =
            {
                updatedRemoteProcessId = r
                updateModelId = p.updateModelId
                progress = p.progress
            }


    let fromRemoteProgress (p : LocalProgressUpdateInfo) l =
            {
                updatedLocalProcessId = l
                updateModelId = p.updateModelId
                progress = p.progress
            }


    type RunningProcessInfo =
        {
            started : DateTime
            runningProcessId : ProcessId
            runningModelId : ModelDataId
            runningQueueId : RunQueueId option
            progress : TaskProgress
        }

        override r.ToString() =
            let (ModelDataId modelDataId) = r.runningModelId
            let s = formatTimeSpan (DateTime.Now - r.started)

            let estCompl =
                match r.progress.estimateEndTime r.started with
                | Some e -> " est. compl.: " + e.ToShortDateString() + ", " + e.ToShortTimeString() + ";"
                | None -> EmptyString

            sprintf "{ running = %s;%s modelDataId = %A; PID = %A; %A }" s estCompl modelDataId r.runningProcessId r.progress


    type ProgressUpdateInfo
        with
        member this.runningProcessInfo =
            {
                started = DateTime.Now
                runningProcessId = this.updatedProcessId
                runningModelId = this.updateModelId
                runningQueueId = None
                progress = this.progress
            }


    type ProcessStartInfo =
        {
            processId : ProcessId
            modelDataId : ModelDataId
            runQueueId : RunQueueId
        }

        member this.runningProcessInfo =
            {
                started = DateTime.Now
                runningProcessId = this.processId
                runningModelId = this.modelDataId
                runningQueueId = Some this.runQueueId
                progress = TaskProgress.NotStarted
            }


    type LocalProcessStartInfo =
        {
            localProcessId : LocalProcessId
            modelDataId : ModelDataId
            runQueueId : RunQueueId
        }

        member this.processStartInfo =
            {
                processId = this.localProcessId |> LocalProcess
                modelDataId = this.modelDataId
                runQueueId = this.runQueueId
            }


    type ProcessResult =
        {
            startInfo : ProcessStartInfo
            exitCode : int
            runTime : int64
            outputs : seq<string>
            errors : seq<string>
        }


    type ProcessStartedInfo =
        {
            calledBackModelId : ModelDataId
            runQueueId : RunQueueId
        }


    type ProcessStartedCallBack =
        {
            notifyOnStarted : ProcessStartInfo -> unit
        }


    type ProcessStartedInfoWithCallBack =
        {
            processStartedInfo : ProcessStartedInfo
            callBack : ProcessStartedCallBack
        }


    type RunInfo =
        {
            run : ProcessStartedInfoWithCallBack -> ProcessStartInfo
            modelDataId : ModelDataId
            runQueueId : RunQueueId
        }


    type ContGenRunnerState =
        {
            runLimit : int
            maxQueueLength : int
            running : RunningProcessInfo[]
            queue : ModelDataId[]
            runningCount : int
            workState : WorkState
            messageCount : int64
            minUsefulEe : MinUsefulEe
        }

        override s.ToString() =
            let q0 = (s.queue |> Array.map (fun e -> e.value.ToString()) |> String.concat "; ")

            let q =
                let x = "length: " + (s.queue.Length.ToString()) + ", "
                if q0 = EmptyString then x + "[]" else x + "[ " + q0 + " ]"

            let r0 = s.running |> Array.map (fun e -> "            " + e.ToString()) |> String.concat Nl
            let r = if r0 = EmptyString then "[]" else Nl + "        [" + Nl + r0 + Nl + "        ]"
            sprintf "{\n    running = %s\n    queue = %s\n    runLimit = %A; runningCount = %A; messageCount = %A; workState = %A; minUsefulEe = %A\n }" r q s.runLimit s.runningCount s.messageCount s.workState s.minUsefulEe.value


    type ContGenConfigParam =
        | SetToIdle
        | SetToCanGenerate
        | RequestShutDown of waitForCompletion : bool
        | SetRunLimit of numberOfCores : int
        | CancelTask of processId : ProcessId
        | SetMinUsefulEe of ee : double


    type IContGenService =
        abstract getState : unit -> ContGenRunnerState
        abstract loadQueue : unit -> unit
        abstract startGenerate : unit -> unit
        abstract updateLocalProgress : LocalProgressUpdateInfo -> unit
        abstract updateRemoteProgress : RemoteProgressUpdateInfo -> unit
        abstract configureService : ContGenConfigParam -> unit
        abstract runModel : ModelDataId -> ModelCommandLineParam -> unit


    let mutable private callCount = -1


    let getServiceState (service : IContGenService) =
        if Interlocked.Increment(&callCount) = 0
        then
            printfn "Getting state at %A ..." DateTime.Now
            let state = service.getState()
            printfn "...state at %A =\n%s\n\n" DateTime.Now (state.ToString())
            if state.queue.Length = 0 then service.startGenerate()
        else
            printfn "Not getting state at %A because callCount = %A." DateTime.Now callCount
            ignore()
        Interlocked.Decrement(&callCount) |> ignore


    type RunProcArgs =
        {
            callBackInfo : ProcessStartedCallBack
            notifyOnStarted : ProcessStartInfo -> unit
            fileName : string
            commandLineArgs : string
            startDir : string option
        }


    let runProc (c : ProcessStartedInfoWithCallBack) filename args startDir =
        let procStartInfo =
            ProcessStartInfo(
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false,
                FileName = filename,
                Arguments = args
            )

        match startDir with | Some d -> procStartInfo.WorkingDirectory <- d | _ -> ()

        let outputs = System.Collections.Generic.List<string>()
        let errors = System.Collections.Generic.List<string>()
        let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) = f args.Data
        let p = new Process(StartInfo = procStartInfo)
        p.OutputDataReceived.AddHandler(DataReceivedEventHandler (outputHandler outputs.Add))
        p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (outputHandler errors.Add))

        let started =
            try
                p.Start()
            with
                | ex ->
                    // TODO kk:20190203 Here we need to notify AsyncRunner that starting the process has failed.
                    // Otherwise runningCount is not decreased.
                    ex.Data.Add("filename", filename)
                    false

        if not started
        then
            printfn "Failed to start process %s" filename

            {
                localProcessId = -1 |> LocalProcessId
                modelDataId = c.processStartedInfo.calledBackModelId
                runQueueId = c.processStartedInfo.runQueueId
            }
        else
            p.PriorityClass <- ProcessPriorityClass.Idle

            let processId = p.Id |> LocalProcessId

            printfn "Started %s with pid %A" p.ProcessName processId
            c.callBack.notifyOnStarted { processId = processId |> LocalProcess; modelDataId = c.processStartedInfo.calledBackModelId; runQueueId = c.processStartedInfo.runQueueId }

            {
                localProcessId = processId
                modelDataId = c.processStartedInfo.calledBackModelId
                runQueueId = c.processStartedInfo.runQueueId
            }


    type RunModelParam =
        {
            exeName : string
            commandLineParam : ModelCommandLineParam
        }


    type RunModelParamWithCallBack =
        {
            runModelParam : RunModelParam
            callBackInfo : ProcessStartedInfoWithCallBack
        }
