namespace OdeSolver

open Microsoft.FSharp.Core
open System

module Solver = 

    type OdeParams = 
        {
            modelName : string
            startTime : double
            endTime : double
            stepSize : double
            eps : double
            noOfOutputPoints : int option
            noOfProgressPoints : int option
        }

        static member defaultValue =
            {
                modelName = String.Empty
                startTime = 0.0
                endTime = 10.0
                stepSize = 0.01
                eps = 0.00001
                noOfOutputPoints = Some 1000
                noOfProgressPoints = Some 100
            }


    type OdeResult = 
        {
            modelName : string
            y0 : double
            noOfOutputPoints : int
            startTime : double
            endTime : double
            t : double[]
            x : double[,]
        }


    type NSolveParam = 
        {
            modelName : string
            tEnd : double
            g : double[] -> double[]
            h : double -> double[]
            y0 : double
        }


    /// F# wrapper around Alglib ODE solver.
    let nSolve (n : NSolveParam) : OdeResult = 
        printfn "nSolve::Starting."
        let i = n.h n.y0

        let mutable progressCount = 0

        let p = { OdeParams.defaultValue with modelName = n.modelName; endTime = n.tEnd }

        let f (x : double[]) (t : double) : double[] = 
            match p.noOfProgressPoints with 
            | Some k when k > 0 && n.tEnd > 0.0 ->
                if t > (double progressCount) * (n.tEnd / (double k))
                then 
                    progressCount <- ((double k) * (t / n.tEnd) |> int) + 1
                    printfn "Step: %A, time: %A, t = %A of %A." progressCount (DateTime.Now) t n.tEnd
            | _ -> ignore()

            n.g x

        let nt = 
            match p.noOfOutputPoints with 
            | Some p when p >= 2 -> p
            | _ -> 2

        let x : array<double> = [| for i in 0..nt -> p.startTime + (p.endTime - p.startTime) * (double i) / (double nt) |]

        //printfn "nSolve::About to call alglib.ndimensional_ode_rp."
        let d = alglib.ndimensional_ode_rp (fun x t y _ -> f x t |> Array.mapi(fun i e -> y.[i] <- e) |> ignore)

        //printfn "nSolve::About to call alglib.odesolverrkck."
        let mutable s = alglib.odesolverrkck(i, x, p.eps, p.stepSize)

        printfn "nSolve::About to call alglib.odesolversolve."
        do alglib.odesolversolve(s, d, null)
        let mutable (m, xtbl, ytbl, rep) = alglib.odesolverresults(s)

        {
            modelName = p.modelName
            y0 = n.y0
            noOfOutputPoints = nt
            startTime = p.startTime
            endTime = p.endTime
            t = xtbl
            x = ytbl
        }
