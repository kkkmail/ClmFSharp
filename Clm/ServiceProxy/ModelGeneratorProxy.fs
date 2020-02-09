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
open Clm.Generator.ClmModelData

module ModelGeneratorProxy =

    type GenerateModelProxy =
        {
            loadParams : ClmTask -> ClmResult<AllParams>
            upsertModelData : ModelData -> UnitResult
            upsertRunQueue : RunQueue -> UnitResult
        }


    type GenerateAllProxy =
        {
            loadIncompleteClmTasks : SolverRunnerAccessInfo -> ListResult<ClmTask>
            generateModel : ClmTask -> UnitResult
        }
