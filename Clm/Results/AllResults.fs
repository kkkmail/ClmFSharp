namespace Clm.Results

open Clm.Substances
open Clm.Distributions
open Clm.ModelParams
open Clm.ReactionRates
open Clm.OdeSolver.Solver

module AllResults = 

    //type OdeResult = 
    //    {
    //        modelName : string
    //        y0 : double
    //        noOfOutputPoints : int
    //        startTime : double
    //        endTime : double
    //        t : double[]
    //        x : double[,]
    //    }

    /// !!! This file grows automatically at the end. Do not modify it without extreme need !!!
    let allModelResults : list<OdeResult> = 
        [
            {
                modelName = ""
                y0 = 0.0
                noOfOutputPoints = 0
                startTime = 0.0
                endTime = 0.0
                t= [||]
                x = 
                    array2D 
                        [| 
                            [||] 
                        |]
            }
        ]
