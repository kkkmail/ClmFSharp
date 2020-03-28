namespace ClmSys

open GeneralData
open MessagingData
open WorkerNodePrimitives
open PartitionerPrimitives

module WorkerNodeData =

    //type NodeInfo =
    //    {
    //        workerNodeName : WorkerNodeName
    //        noOfCores : int
    //        nodePriority : WorkerNodePriority
    //    }


    type WorkerNodeInfo =
        {
            workerNodeId : WorkerNodeId
            workerNodeName : WorkerNodeName
            partitionerId : PartitionerId
            noOfCores : int
            nodePriority : WorkerNodePriority
            isInactive : bool
        }


    ///// Worker Node MessagingClientId + Messaging Server acces info.
    //type WorkNodeMsgAccessInfo =
    //    {
    //        workerNodeId : WorkerNodeId
    //        msgSvcAccessInfo : MessagingServiceAccessInfo
    //    }
    //
    //    member this.messagingClientAccessInfo =
    //        {
    //            msgClientId = this.workerNodeId.messagingClientId
    //            msgSvcAccessInfo = this.msgSvcAccessInfo
    //        }


    //type WorkerNodeServiceAccessInfo =
    //    {
    //        nodeInfo : NodeInfo
    //        workNodeMsgAccessInfo : WorkNodeMsgAccessInfo
    //        partitionerId : PartitionerId
    //        isInactive : bool
    //    }
    //
    //    member w.workerNodeInfo =
    //        {
    //            workerNodeId = w.workNodeMsgAccessInfo.workerNodeId
    //            nodeInfo = w.nodeInfo
    //        }

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

