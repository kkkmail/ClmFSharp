namespace OdeSolver

open System
open System.IO
open Clm.Substances
open Clm.ModelParams
open Clm.DataLocation
open Clm.OdeSolver.Solver
open Microsoft.FSharp.Core

module ResultSerialization = 

    let saveResults (o : OdeResult) = 
        ignore()
