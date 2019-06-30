namespace OdeSolver

open Microsoft.FSharp.Core
open System
open ClmSys.GeneralData


module Solver =

    type OdeParams =
        {
            modelDataId : Guid
            startTime : double
            endTime : double
            stepSize : double
            eps : double
            noOfOutputPoints : int option
            noOfProgressPoints : int option
        }

        static member defaultValue modelDataId startTime endTime =
            {
                modelDataId = modelDataId
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


    type OdeCombinedResult =
        {
            completed : List<OdeResult>
        }


    type OdeController =
        {
            partition : OdeParams -> (OdeParams * OdeParams)
        }


    type NSolveParam =
        {
            modelDataId : Guid
            tStart : double
            tEnd : double
            g : double[] -> double[]
            h : double -> double[]
            y0 : double
            progressCallBack : (decimal -> unit) option
            chartCallBack : (double -> double[] -> unit) option
        }

        member p.next tEndNew = { p with tStart = p.tEnd; tEnd = tEndNew }


    let calculateProgress r m = (decimal (max 0 (r - 1))) / (decimal m)


    let estCompl s r m =
        match estimateEndTime (calculateProgress r m) s with
        | Some e -> " est. compl.: " + e.ToShortDateString() + ", " + e.ToShortTimeString() + ","
        | None -> EmptyString


    /// F# wrapper around Alglib ODE solver.
    let nSolve (n : NSolveParam) : OdeResult =
        printfn "nSolve::Starting."
        let start = DateTime.Now
        let i = n.h n.y0
        let mutable progressCount = 0
        let mutable outputCount = 0
        let p = OdeParams.defaultValue n.modelDataId n.tStart n.tEnd

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

            n.g x

        let nt = 2
        let x : array<double> = [| for i in 0..nt -> p.startTime + (p.endTime - p.startTime) * (double i) / (double nt) |]
        let d = alglib.ndimensional_ode_rp (fun x t y _ -> f x t |> Array.mapi(fun i e -> y.[i] <- e) |> ignore)
        let mutable s = alglib.odesolverrkck(i, x, p.eps, p.stepSize)
        do alglib.odesolversolve(s, d, null)
        let mutable (m, xtbl, ytbl, rep) = alglib.odesolverresults(s)

        {
            startTime = p.startTime
            endTime = p.endTime
            xEnd = ytbl.[nt - 1, *]
        }

    type NSolvePartitionParam =
        {
            nSolveParam : NSolveParam
            controller : OdeController
        }


    let nSolvePartitioned (p : NSolvePartitionParam) : unit =
        

        ignore()
