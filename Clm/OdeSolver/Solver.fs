namespace OdeSolver

open Microsoft.FSharp.Core
open System
open ClmSys.GeneralData


module Solver =

    type EeData =
        {
            maxEe : double
            maxAverageEe : double
            maxWeightedAverageAbsEe : double
            maxLastEe : double
        }


    type OdeParams =
        {
            startTime : double
            endTime : double
            stepSize : double
            eps : double
            noOfOutputPoints : int option
            noOfProgressPoints : int option
        }

        static member defaultValue startTime endTime =
            {
                startTime = startTime
                endTime = endTime
                stepSize = 0.01
                eps = 0.00001
                noOfOutputPoints = Some 1000
                noOfProgressPoints = Some 100
            }


    type OdeResult =
        {
            startTime : double
            endTime : double
            xEnd : double[]
        }


    type NSolveParam =
        {
            modelDataId : Guid
            tStart : double
            tEnd : double
            derivative : double[] -> double[]
            initialValues : double[]
            progressCallBack : (decimal -> unit) option
            chartCallBack : (double -> double[] -> unit) option
            getEeData : (unit -> EeData) option
        }

        member p.next tEndNew initValNew = { p with tStart = p.tEnd; tEnd = tEndNew; initialValues = initValNew }


    let calculateProgress r m = (decimal (max 0 (r - 1))) / (decimal m)


    let estCompl s r m =
        match estimateEndTime (calculateProgress r m) s with
        | Some e -> " est. compl.: " + e.ToShortDateString() + ", " + e.ToShortTimeString() + ","
        | None -> EmptyString


    /// F# wrapper around Alglib ODE solver.
    let nSolve (n : NSolveParam) : OdeResult =
        printfn "nSolve::Starting."
        let start = DateTime.Now
        let mutable progressCount = 0
        let mutable outputCount = 0
        let p = OdeParams.defaultValue n.tStart n.tEnd

        let notify t r m =
            match n.progressCallBack with
            | Some c -> calculateProgress r m |> c
            | None -> ignore()

        let notifyChart t x =
            match n.chartCallBack with
            | Some c -> c t x
            | None -> ignore()

        let f (x : double[]) (t : double) : double[] =
            match p.noOfProgressPoints with
            | Some k when k > 0 && n.tEnd > 0.0 ->
                if t > (double progressCount) * (n.tEnd / (double k))
                then
                    progressCount <- ((double k) * (t / n.tEnd) |> int) + 1
                    printfn "Step: %A, time: %A,%s t: %A of %A, modelDataId: %A." progressCount (DateTime.Now) (estCompl start progressCount k) t n.tEnd n.modelDataId
                    notify t progressCount k
            | _ -> ignore()

            match p.noOfOutputPoints with
            | Some k when k > 0 ->
                if t > (double outputCount) * (n.tEnd / (double k))
                then
                    outputCount <- ((double k) * (t / n.tEnd) |> int) + 1
                    notifyChart t x
            | _ -> ignore()

            n.derivative x

        let nt = 2
        let x : array<double> = [| for i in 0..nt -> p.startTime + (p.endTime - p.startTime) * (double i) / (double nt) |]
        let d = alglib.ndimensional_ode_rp (fun x t y _ -> f x t |> Array.mapi(fun i e -> y.[i] <- e) |> ignore)
        let mutable s = alglib.odesolverrkck(n.initialValues, x, p.eps, p.stepSize)
        do alglib.odesolversolve(s, d, null)
        let mutable (m, xtbl, ytbl, rep) = alglib.odesolverresults(s)

        {
            startTime = p.startTime
            endTime = p.endTime
            xEnd = ytbl.[nt - 1, *]
        }


    type PartitionType =
        | InsideInterval
        | EndOfInterval
        | OutsideInterval


    type PartitionInfo =
        {
            partitionType : PartitionType
            getNSolveParam : double[] -> NSolveParam
        }


    let defaultPartition (n : NSolveParam) : List<PartitionInfo> =
        let p =
            [
                (30.0, InsideInterval)
                (75.0, InsideInterval)
                (150.0, InsideInterval)
                (250.0, EndOfInterval)
                (400.0, OutsideInterval)
                (600.0, OutsideInterval)
                (1000.0, OutsideInterval)
            ]

        let s =
            match p |> List.tryPick (fun (a, b) -> match b with | EndOfInterval -> Some a | _ -> None) with
            | Some a -> a
            | None -> 250.0

        p
        |> List.mapi (fun i (a, b) ->
                    {
                        partitionType = b
                        getNSolveParam =
                            if i = 0
                            then fun x -> { n with initialValues = x; tStart = n.tStart; tEnd = n.tEnd * a / s }
                            else n.next (n.tEnd * a / s)
                    })


    let defaultContinueRun (n : NSolveParam) (p : PartitionInfo) =
        failwith ""


    type OdeController =
        {
            partition : NSolveParam -> List<PartitionInfo>
            continueRun : NSolveParam -> PartitionInfo -> bool // This function is NOT pure because there is a MailboxProcessor behind it.
        }

        static member defaultValue =
            {
                partition = defaultPartition
                continueRun = defaultContinueRun
            }


    type NSolvePartitionParam =
        {
            nSolveParam : NSolveParam
            controller : OdeController
        }


    let nSolvePartitioned (p : NSolvePartitionParam) : unit =
        let d = p.controller.partition p.nSolveParam

        let x =
            d
            |> List.fold(fun (a, b) e ->
                            if b
                            then
                                let r = e.getNSolveParam a |> nSolve
                                (r.xEnd, p.controller.continueRun p.nSolveParam e)
                            else (a, b)) (p.nSolveParam.initialValues, true)

        ignore()
