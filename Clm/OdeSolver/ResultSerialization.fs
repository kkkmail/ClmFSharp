namespace OdeSolver

open System
open System.IO
open Clm.Substances
open Clm.ModelParams
open Clm.DataLocation
open Clm.OdeSolver.Solver
open Clm.Generator.FSharpCodeExt
open Microsoft.FSharp.Core

module ResultSerialization = 

    type OdeResult 
        with 
        member o.toFSharpCode (shift : string) = 
            shift + "        [" + Nl +
            shift + "            {" + Nl +
            shift + "                modelName = \"" + o.modelName + "\"" + Nl +
            shift + "                y0 = " + (doubleFSharpString o.y0) + Nl +
            shift + "                noOfOutputPoints = " + (o.noOfOutputPoints.ToString()) + Nl +
            shift + "                startTime = " + (doubleFSharpString o.startTime) + Nl +
            shift + "                endTime = " + (doubleFSharpString o.endTime) + Nl +
            shift + "                t= [||]" + Nl +
            shift + "                x = " + Nl +
            shift + "                    array2D " + Nl +
            shift + "                        [| " + Nl +
            shift + "                            [||] " + Nl +
            shift + "                        |]" + Nl +
            shift + "            }" + Nl +
            shift + "        ]" + Nl

    let saveResults (o : OdeResult) = 
        ignore()
