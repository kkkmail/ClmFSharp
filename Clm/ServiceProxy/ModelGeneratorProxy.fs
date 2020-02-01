namespace ServiceProxy

open Clm.ModelParams
open Clm.CalculationData
open DbData.Configuration
open DbData.DatabaseTypes
open NoSql.FileSystemTypes
open ClmSys.Registry
open PartitionerServiceInfo.ServiceInfo
open ClmSys.GeneralPrimitives
open ClmSys.ContGenPrimitives
open ClmSys.ClmErrors
open ClmSys.SolverRunnerData
open ClmSys.WorkerNodePrimitives

module ModelGeneratorProxy =

    type ModelGeneratorProxy =
        {
            generate : unit -> ListResult<RunQueueId>
        }
