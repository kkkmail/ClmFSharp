namespace ContGen

open System
open ClmSys.GeneralData
open Clm.ModelParams
open Clm.Generator.ClmModelData
open Clm.Generator.ClmModel
open Clm.CommandLine
open Clm.CalculationData
open ContGenServiceInfo.ServiceInfo
open ServiceProxy.Runner
open DbData.Configuration

module Partitioner =

// type RemoteRunnerConfig =
    //{
    //    connectionString : ConnectionString
    //    runModel : RunModelParam -> ProcessStartInfo
    //}


    let runModelImpl (p : RunModelParam) : ProcessStartInfo =
        failwith ""


    let createRemoteRunner x : RemoteRunnerConfig =
        {
            connectionString = clmConnectionString
            runModel = runModelImpl
        }
