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

module PartitionerProxy =

    type PartitionerProxyInfo =
        {
            partitionerConnectionString : ConnectionString
            resultLocation : string
        }

        static member defaultValue =
            {
                partitionerConnectionString = clmConnectionString
                resultLocation = DefaultResultLocationFolder
            }


    type PartitionerProxy =
        {
            loadModelData : SolverRunnerAccessInfo -> ModelDataId -> ClmResult<ModelData>
            saveResultData : ResultDataWithId -> UnitResult
            tryLoadResultData : ResultDataId -> ClmResult<ResultDataWithId option>

            saveCharts : ChartInfo -> UnitResult

            saveRunModelParamWithRemoteId : RunModelParamWithRemoteId -> UnitResult
            loadRunModelParamWithRemoteId : RemoteProcessId -> ClmResult<RunModelParamWithRemoteId>
            loadAllRunModelParamWithRemoteId : unit -> ListResult<RunModelParamWithRemoteId>
            tryDeleteRunModelParamWithRemoteId : RemoteProcessId -> UnitResult

            saveWorkerNodeState : WorkerNodeState -> UnitResult
            loadAllWorkerNodeState : unit -> ListResult<WorkerNodeState>
            tryDeleteWorkerNodeState : WorkerNodeId -> UnitResult
        }


        static member create (i : PartitionerProxyInfo) =
            let name = partitionerServiceName
            let connectionString = i.partitionerConnectionString

            {
                loadModelData = loadModelData connectionString
                saveResultData = saveResultData connectionString
                tryLoadResultData = tryLoadResultData connectionString

                saveCharts = fun (c : ChartInfo) -> saveLocalChartInfo (Some (i.resultLocation, c.defaultValueId)) c

                saveRunModelParamWithRemoteId = saveRunModelParamWithRemoteIdFs name
                loadRunModelParamWithRemoteId = loadRunModelParamWithRemoteIdFs name
                loadAllRunModelParamWithRemoteId = loadeRunModelParamWithRemoteIdAllFs name
                tryDeleteRunModelParamWithRemoteId = tryDeleteRunModelParamWithRemoteIdFs name

                saveWorkerNodeState = saveWorkerNodeStateFs name
                loadAllWorkerNodeState = loadWorkerNodeStateAllFs name
                tryDeleteWorkerNodeState = tryDeleteWorkerNodeStateFs name
            }
