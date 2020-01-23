namespace ServiceProxy

open ClmSys.GeneralData
open Clm.ModelParams
open Clm.CalculationData
open DbData.Configuration
open DbData.DatabaseTypes
open NoSql.FileSystemTypes
open ClmSys.Registry
open PartitionerServiceInfo.ServiceInfo
open ClmSys.MessagingData
open ClmSys.GeneralErrors

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
            tryLoadRunModelParamWithRemoteId : RemoteProcessId -> ClmResult<RunModelParamWithRemoteId>
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

                saveCharts = fun (c : ChartInfo) -> trySaveLocalChartInfo (Some (i.resultLocation, c.defaultValueId)) c

                saveRunModelParamWithRemoteId = trySaveRunModelParamWithRemoteIdFs name
                tryLoadRunModelParamWithRemoteId = tryLoadRunModelParamWithRemoteIdFs name
                loadAllRunModelParamWithRemoteId = tryLoadeRunModelParamWithRemoteIdAllFs name
                tryDeleteRunModelParamWithRemoteId = tryDeleteRunModelParamWithRemoteIdFs name

                saveWorkerNodeState = trySaveWorkerNodeStateFs name
                loadAllWorkerNodeState = tryLoadWorkerNodeStateAllFs name
                tryDeleteWorkerNodeState = tryDeleteWorkerNodeStateFs name
            }
