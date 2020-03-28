namespace ContGenServiceInfo

open System
open System.Diagnostics
open ClmSys.GeneralData
open System.Threading
open Clm.ModelParams
open ClmSys.VersionInfo
open ClmSys.ContGenData
open ClmSys.GeneralErrors
open System.Runtime.Remoting.Channels.Tcp
open ClmSys.GeneralPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.ContGenPrimitives
open ClmSys.ClmErrors

module ServiceInfo =

    let contGenServiceProgramName = "ContGenService.exe"


    type RunningProcessData =
        {
            modelDataId : ModelDataId
            defaultValueId : ClmDefaultValueId
            runQueueId : RunQueueId
            workerNodeId : WorkerNodeId
            commandLineParams : ModelCommandLineParam
        }

    //    member this.toResultDataId() = this.runQueueId.toResultDataId()
    //    member this.toRemoteProcessId() = this.runQueueId.toRemoteProcessId()


    //type ProcessStartedInfo =
    //    {
    //        processId : ProcessId
    //        runningProcessData : RunningProcessData
    //    }


    type ProgressUpdateInfo =
        {
            runQueueId : RunQueueId
            progress : TaskProgress
        }


    //type LocalProgressUpdateInfo =
    //    {
    //        localProcessId : LocalProcessId
    //        runningProcessData : RunningProcessData
    //        progress : TaskProgress
    //    }

    //    member this.toProgressUpdateInfo() =
    //        {
    //            processStartedInfo =
    //                {
    //                    processId = this.localProcessId |> LocalProcess
    //                    runningProcessData = this.runningProcessData
    //                }
    //            progress = this.progress
    //        }


    //type RemoteProgressUpdateInfo =
    //    {
    //        remoteProcessId : RemoteProcessId
    //        runningProcessData : RunningProcessData
    //        progress : TaskProgress
    //    }

    //    member this.toProgressUpdateInfo() =
    //        {
    //            processStartedInfo =
    //                {
    //                    processId = this.remoteProcessId |> RemoteProcess
    //                    runningProcessData = this.runningProcessData
    //                }
    //            progress = this.progress
    //        }


    //let fromLocalProgress (p : LocalProgressUpdateInfo) =
    //        {
    //            remoteProcessId = p.runningProcessData.toRemoteProcessId()
    //            runningProcessData = p.runningProcessData
    //            progress = p.progress
    //        }


    //let fromRemoteProgress (p : LocalProgressUpdateInfo) l =
    //        {
    //            localProcessId = l
    //            runningProcessData = p.runningProcessData
    //            progress = p.progress
    //        }


    type RunningProcessInfo =
        {
            started : DateTime
            progressUpdateInfo : ProgressUpdateInfo
        }

        //override r.ToString() =
        //    let (ModelDataId modelDataId) = r.progressUpdateInfo.processStartedInfo.runningProcessData.modelDataId
        //    let s = (DateTime.Now - r.started).ToString("d\.hh\:mm")
        //
        //    let estCompl =
        //        match r.progressUpdateInfo.progress.estimateEndTime r.started with
        //        | Some e -> " ETC: " + e.ToString("yyyy-MM-dd.HH:mm") + ";"
        //        | None -> EmptyString
        //
        //    sprintf "{ T: %s;%s DF: %s; MDID: %A; PID: %s; %A }"
        //        s estCompl (r.progressUpdateInfo.processStartedInfo.runningProcessData.defaultValueId.ToString()) modelDataId (r.progressUpdateInfo.processStartedInfo.processId.ToString()) r.progressUpdateInfo.progress


    type ProgressUpdateInfo
        with
        member this.toRunningProcessInfo() =
            {
                started = DateTime.Now
                progressUpdateInfo = this
            }


    //type ProcessStartedInfo
    //    with

    //    member this.toRunningProcessInfo() =
    //        {
    //            started = DateTime.Now
    //            progressUpdateInfo =
    //                {
    //                    processStartedInfo =
    //                        {
    //                            processId = this.processId
    //                            runningProcessData = this.runningProcessData
    //                        }
    //                    progress = TaskProgress.NotStarted
    //                }
    //        }


    //type LocalProcessStartedInfo =
    //    {
    //        localProcessId : LocalProcessId
    //        runningProcessData : RunningProcessData
    //    }

    //    member this.toProcessStartedInfo() =
    //        {
    //            processId = this.localProcessId |> LocalProcess
    //            runningProcessData = this.runningProcessData
    //        }


    //type ProcessStartedOkResult =
    //    | AlreadyCompleted of ClmError option
    //    | StartedSuccessfully of ProcessStartedInfo * ClmError option


    //type ProcessStartedResult = ClmResult<ProcessStartedOkResult>


    //let combineResult (result : ProcessStartedResult) (e : UnitResult) =
    //    let addError f x = [ Some f ; x ] |> List.choose id |> foldErrors

    //    match e with
    //    | Ok() -> result
    //    | Error f ->
    //        match result with
    //        | Ok (AlreadyCompleted x) -> addError f x |> AlreadyCompleted |> Ok
    //        | Ok (StartedSuccessfully (i, x)) -> StartedSuccessfully (i, addError f x) |> Ok
    //        | Error g -> Error (f + g)


    //type ProcessResult =
    //    {
    //        startInfo : ProcessStartedInfo
    //        exitCode : int
    //        runTime : int64
    //        outputs : seq<string>
    //        errors : seq<string>
    //    }


    //type RunInfo =
    //    {
    //        run : RunningProcessData -> ProcessStartedResult
    //        processToStartInfo : RunningProcessData
    //    }


    type ContGenConfigParam =
        | SetToIdle
        | SetToCanGenerate
        | RequestShutDown of waitForCompletion : bool
        | SetRunLimit of numberOfCores : int
        | CancelTask of runQueueId : RunQueueId
        | SetMinUsefulEe of ee : double


    type ContGenShutDownInfo =
        {
            contGenTcpChannel : TcpChannel
        }


    let mutable private callCount = -1


    let getServiceState (getState : unit -> (list<RunQueue> * UnitResult)) =
        if Interlocked.Increment(&callCount) = 0
        then
            try
                printfn "Getting state at %s ..." (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss"))
                let (q, e) = getState()
                let r0 = q |> List.sortBy (fun e -> e.progress) |> List.map (fun e -> "      " + e.ToString()) |> String.concat Nl
                let r = if r0 = EmptyString then "[]" else Nl + "    [" + Nl + r0 + Nl + "    ]"
                printfn "... state at %s\n{\n  running = %s\n  runningCount = %A\n }"  (DateTime.Now.ToString("yyyy-MM-dd.HH:mm:ss")) r q.Length
            with
            | e -> printfn "Exception occurred: %A" e
        else
            printfn "Not getting state at %A because callCount = %A." DateTime.Now callCount
            ignore()

        Interlocked.Decrement(&callCount) |> ignore
        Ok()


    //type RunProcArgs =
    //    {
    //        fileName : string
    //        commandLineArgs : string
    //        startDir : string option
    //    }

    // !!! kk:20200322 - DO NOT DELETE !!!
    //let runProc (c : RunningProcessData) filename args startDir =
    //    let procStartInfo =
    //        ProcessStartInfo(
    //            RedirectStandardOutput = true,
    //            RedirectStandardError = true,
    //            UseShellExecute = false,
    //            FileName = filename,
    //            Arguments = args
    //        )
    //
    //    match startDir with | Some d -> procStartInfo.WorkingDirectory <- d | _ -> ()
    //
    //    let outputs = System.Collections.Generic.List<string>()
    //    let errors = System.Collections.Generic.List<string>()
    //    let outputHandler f (_sender:obj) (args:DataReceivedEventArgs) = f args.Data
    //    let p = new Process(StartInfo = procStartInfo)
    //    p.OutputDataReceived.AddHandler(DataReceivedEventHandler (outputHandler outputs.Add))
    //    p.ErrorDataReceived.AddHandler(DataReceivedEventHandler (outputHandler errors.Add))
    //
    //    try
    //        p.Start() |> ignore
    //        p.PriorityClass <- ProcessPriorityClass.Idle
    //        let processId = p.Id |> LocalProcessId
    //
    //        printfn "Started %s with pid %A" p.ProcessName processId
    //
    //        {
    //            localProcessId = processId
    //            runningProcessData = c
    //        }
    //        |> Ok
    //    with
    //    | ex ->
    //        printfn "Failed to start process %s" filename
    //        ex.Data.["filename"] <- filename
    //        ex.Data.["arguments"] <- args
    //        FailedToStart ex |> Error


    //type RunModelParam =
    //    {
    //        exeName : string
    //        callBackInfo : RunningProcessData
    //    }
