namespace PartitionerServiceInfo
open ClmSys.GeneralData

module ServiceInfo =


    [<Literal>]
    let PartitionerServiceName = "PartitionerService"

    [<Literal>]
    let PartitionerServiceProgramName = "PartitionerService.exe"


    let getServiceUrl (i : PartitionerAccessInfo) =
        getServiceUrlImpl i.partitionerAccessInfo.serviceAddress.value i.partitionerAccessInfo.servicePort.value PartitionerServiceName


    type RegisterContGenServiceData =
        {
            dummy : int
        }


    type UpdateContGenServiceData =
        {
            dummy : int
        }


    type ContGenRequestMessage =
        | RegisterContGenService of RegisterContGenServiceData
        | UpdateContGenService of UpdateContGenServiceData


    type ContGenRequest =
        {
            contGenRequestMessages : ContGenRequestMessage[]
        }


    type NotifyOfProgressData =
        {
            dummy : int
        }


    type OutputResultData =
        {
            dummy : int
        }


    type ContGenResponseMessage =
        | NotifyOfProgressContGen of NotifyOfProgressData
        | OutputResultContGen of OutputResultData


    type ContGenResponse =
        {
            contGenResponseMessages : ContGenResponseMessage[]
        }


    type RegisterRunnerData =
        {
            dummy : int
        }


    type UpdateRunnerData =
        {
            dummy : int
        }


    type SolverRunnerRequestMessage =
        | RegisterRunner of RegisterRunnerData
        | UpdateRunner of UpdateRunnerData
        | NotifyOfProgress of NotifyOfProgressData
        | OutputResult of OutputResultData


    type SolverRunnerRequest =
        {
            solverRunnerRequestMessages : SolverRunnerRequestMessage[]
        }


    type UpdateDefaultsData =
        {
            dummy : int
        }


    type RunModelData =
        {
            dummy : int
        }


    type CancelModealData =
        {
            dummy : int
        }


    type SolverRunnerResponseMessage =
        | UpdateDefaults of UpdateDefaultsData
        | RunModel of RunModelData
        | CancelModel of CancelModealData


    type SolverRunnerResponse =
        {
            solverRunnerResponseMessages : SolverRunnerResponseMessage[]
        }


    type IPartitionerService =
        abstract contGenMessageExchange : ContGenRequest -> ContGenResponse
        abstract solverRunnerMessageExchange : SolverRunnerRequest -> SolverRunnerResponse
