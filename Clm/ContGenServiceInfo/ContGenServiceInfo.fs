namespace ContGenServiceInfo

open System
open ClmSys.GeneralData
open System.Threading
open Clm.ModelParams
open ClmSys.GeneralPrimitives
open ClmSys.WorkerNodePrimitives
open ClmSys.ContGenPrimitives
open ClmSys.ClmErrors
open System.ServiceModel

module ServiceInfo =

    let contGenServiceProgramName = "ContGenService.exe"


    [<Literal>]
    let ContGenWcfServiceName = "ContGenWcfService"


    type RunningProcessData =
        {
            modelDataId : ModelDataId
            defaultValueId : ClmDefaultValueId
            runQueueId : RunQueueId
            workerNodeId : WorkerNodeId
            commandLineParams : ModelCommandLineParam
        }


    type ProgressUpdateInfo =
        {
            runQueueId : RunQueueId
            progress : TaskProgress
        }


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


    type IContGenService =
        abstract tryCancelRunQueue : RunQueueId -> UnitResult


    /// https://gist.github.com/dgfitch/661656
    [<ServiceContract(ConfigurationName = ContGenWcfServiceName)>]
    type IContGenWcfService =

        [<OperationContract(Name = "tryCancelRunQueue")>]
        abstract tryCancelRunQueue : q:byte[] -> byte[]


    //type ContGenWcfCommunicator = (IContGenWcfService -> byte[] -> byte[])
