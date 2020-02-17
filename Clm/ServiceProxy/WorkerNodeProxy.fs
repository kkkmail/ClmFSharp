namespace ServiceProxy

open NoSql.FileSystemTypes
open ClmSys.Registry
open MessagingServiceInfo.ServiceInfo
open ContGenServiceInfo.ServiceInfo
open Clm.CalculationData
open Clm.ModelParams
open ClmSys.GeneralErrors
open ClmSys.GeneralPrimitives
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.SolverRunnerData
open ClmSys.GeneralData


module WorkerNodeProxy =

    let getCommandLine (p : RunModelParam) r e =
        let data =
            {
                modelDataId = p.callBackInfo.modelDataId
                resultDataId = p.callBackInfo.runQueueId.toResultDataId()
                workerNodeId = p.callBackInfo.workerNodeId
                minUsefulEe = e
                remote = r
            }

        let commandLineParams = p.callBackInfo.commandLineParams.toCommandLine data
        commandLineParams


    let runLocalModel (p : RunModelParam) r e =
        let fullExeName = getExeName p.exeName
        let commandLineParams = getCommandLine p r e
        printfn "runModel::commandLineParams = %A\n" commandLineParams
        runProc p.callBackInfo fullExeName commandLineParams None


    type StorageType =
        | LocalStorage of ConnectionString
        | RemoteStorage


    type WorkerNodeProxyData =
        {
            minUsefulEe : MinUsefulEe
        }

        static member defaultValue =
            {
                minUsefulEe = MinUsefulEe.defaultValue
            }

    type WorkerNodeProxy =
        {
            saveWorkerNodeRunModelData : WorkerNodeRunModelData -> UnitResult
            loadWorkerNodeRunModelData : RemoteProcessId -> ClmResult<WorkerNodeRunModelData>
            tryDeleteWorkerNodeRunModelData : RemoteProcessId -> UnitResult
            runModel : RunModelParam ->  Result<LocalProcessStartedInfo, ProcessStartedError>
            getCommandLine : RunModelParam -> string
            loadAllWorkerNodeRunModelData : unit -> ListResult<WorkerNodeRunModelData>

            saveModelData : ModelData -> UnitResult
            tryDeleteModelData : ModelDataId -> UnitResult

            loadResultData : ResultDataId -> ClmResult<ResultDataWithId>
            tryDeleteResultData : ResultDataId -> UnitResult
            loadAllResultData : unit -> ListResult<ResultDataWithId>

            loadChartInfo : ResultDataId -> ClmResult<ChartInfo>
            tryDeleteChartInfo : ResultDataId -> UnitResult
        }

        static member create (i : WorkerNodeProxyData) =
            let name = workerNodeServiceName

            {
                saveWorkerNodeRunModelData = saveWorkerNodeRunModelDataFs name
                loadWorkerNodeRunModelData = loadWorkerNodeRunModelDataFs name
                tryDeleteWorkerNodeRunModelData = tryDeleteWorkerNodeRunModelDataFs name
                runModel = fun p -> runLocalModel p true i.minUsefulEe
                getCommandLine = fun p -> getCommandLine p true i.minUsefulEe
                loadAllWorkerNodeRunModelData = loadWorkerNodeRunModelDataAllFs name

                // These ones are needed for SolverRunner interop.
                // Note that the "name" is different here.
                saveModelData = saveModelDataFs solverRunnerName
                tryDeleteModelData = tryDeleteModelDataFs solverRunnerName

                loadResultData = loadResultDataFs solverRunnerName
                tryDeleteResultData = tryDeleteResultDataFs solverRunnerName
                loadAllResultData = loadResultDataAllFs solverRunnerName

                loadChartInfo = loadChartInfoFs solverRunnerName
                tryDeleteChartInfo = tryDeleteChartInfoFs solverRunnerName
            }
