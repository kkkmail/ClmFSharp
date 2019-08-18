namespace ServiceProxy

open ClmSys.Retry
open ClmSys.GeneralData
open Clm.ModelParams
open DbData.Configuration
open DbData.DatabaseTypes
open ContGenServiceInfo.ServiceInfo

module PartitionerProxy =

    type PartitionerProxyInfo =
        {
            partitionerConnectionString : ConnectionString
        }

        static member defaultValue =
            {
                partitionerConnectionString = clmConnectionString
            }


    type PartitionerProxy(i : PartitionerProxyInfo) =
        let logError e = printfn "Error: %A" e
        let tryDbFun c f = tryDbFun logError c f
        let connectionString = i.partitionerConnectionString
        let tryLoadModelDataImpl a m = tryDbFun connectionString (tryLoadModelData a m) |> Option.bind id

        member __.tryLoadModelData i m = tryLoadModelDataImpl i m
