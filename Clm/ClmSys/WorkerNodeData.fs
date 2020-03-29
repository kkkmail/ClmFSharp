namespace ClmSys

open GeneralData
open MessagingData
open WorkerNodePrimitives
open PartitionerPrimitives

module WorkerNodeData =

    type WorkerNodeInfo =
        {
            workerNodeId : WorkerNodeId
            workerNodeName : WorkerNodeName
            partitionerId : PartitionerId
            noOfCores : int
            nodePriority : WorkerNodePriority
            isInactive : bool
        }


    type WorkerNodeServiceAccessInfo =
        {
            workerNodeServiceAddress : WorkerNodeServiceAddress
            workerNodeServicePort : WorkerNodeServicePort
            workerNodeServiceName : WorkerNodeServiceName
        }

        member s.serviceName = s.workerNodeServiceName.value.value
        member s.serviceUrl = getServiceUrlImpl s.workerNodeServiceAddress.value s.workerNodeServicePort.value s.serviceName
        member s.wcfServiceName = toValidServiceName s.serviceName
        member s.wcfServiceUrl = getWcfServiceUrlImpl s.workerNodeServiceAddress.value s.workerNodeServicePort.value s.wcfServiceName


    type WorkerNodeServiceInfo =
        {
            workerNodeInfo : WorkerNodeInfo
            workerNodeServiceAccessInfo : WorkerNodeServiceAccessInfo
            messagingServiceAccessInfo : MessagingServiceAccessInfo
        }

        member this.messagingClientAccessInfo =
            {
                msgClientId = this.workerNodeInfo.workerNodeId.messagingClientId
                msgSvcAccessInfo = this.messagingServiceAccessInfo
            }

