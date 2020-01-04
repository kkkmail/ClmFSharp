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
            tryLoadModelData : SolverRunnerAccessInfo -> ModelDataId -> Result<ModelData, ClmError>
            saveResultData : ResultDataWithId -> Result<unit, ClmError>
            tryLoadResultData : ResultDataId -> Result<ResultDataWithId, ClmError>

            saveCharts : ChartInfo -> Result<unit, ClmError>

            saveRunModelParamWithRemoteId : RunModelParamWithRemoteId -> Result<unit, ClmError>
            tryLoadRunModelParamWithRemoteId : RemoteProcessId -> Result<RunModelParamWithRemoteId, ClmError>
            loadAllRunModelParamWithRemoteId : unit -> Result<list<Result<RunModelParamWithRemoteId, ClmError>>, ClmError>
            tryDeleteRunModelParamWithRemoteId : RemoteProcessId -> Result<unit, ClmError>

            saveWorkerNodeState : WorkerNodeState -> Result<unit, ClmError>
            loadAllWorkerNodeState : unit -> Result<list<Result<WorkerNodeState, ClmError>>, ClmError>
            tryDeleteWorkerNodeState : WorkerNodeId -> Result<unit, ClmError>
        }


        static member create (i : PartitionerProxyInfo) =
            let name = partitionerServiceName
            let connectionString = i.partitionerConnectionString

            {
                tryLoadModelData = tryLoadModelData connectionString
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
