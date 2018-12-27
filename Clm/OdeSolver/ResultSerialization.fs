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
            shift + "                t= " + (arrayToFSharpString o.t String.Empty) + Nl +
            shift + "                x = " + Nl +
            shift + "                    array2D " + Nl + (array2DToFSharpString o.x "                    ") + 
            shift + "            }" + Nl +
            shift + "        ]" + Nl + Nl


    let saveResults (fileName : string) (o : OdeResult) = 
        File.AppendAllLines(fileName, [ "        @" + Nl + o.toFSharpCode String.Empty ])
