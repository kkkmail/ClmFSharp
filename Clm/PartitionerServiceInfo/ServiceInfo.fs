namespace PartitionerServiceInfo
open ClmSys.GeneralData

module ServiceInfo =


    [<Literal>]
    let PartitionerServiceName = "PartitionerService"

    [<Literal>]
    let ProgramName = "PartitionerService.exe"


    let private getServiceUrlImpl serviceAddress (servicePort : int) serviceName =
        "tcp://" + serviceAddress + ":" + (servicePort.ToString()) + "/" + serviceName


    let getServiceUrl (i : PartitionerAccessInfo) =
        getServiceUrlImpl i.partitionerAddress.value i.partitionerPort.value PartitionerServiceName

