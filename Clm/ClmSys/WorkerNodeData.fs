namespace ClmSys

open GeneralData
open MessagingData
open WorkerNodePrimitives
open PartitionerPrimitives
open ContGenPrimitives

module WorkerNodeData =

    type NodeInfo =
        {
            workerNodeName : WorkerNodeName
            noOfCores : int
            nodePriority : WorkerNodePriority
        }


    type WorkerNodeInfo =
        {
            workerNodeId : WorkerNodeId
            nodeInfo : NodeInfo
        }


    /// Worker Node MessagingClientId + Messaging Server acces info.
    type WorkNodeMsgAccessInfo =
        {
            workerNodeId : WorkerNodeId
            msgSvcAccessInfo : ServiceAccessInfo
        }

        member this.messagingClientAccessInfo =
            {
                msgClientId = this.workerNodeId.messagingClientId
                msgSvcAccessInfo = this.msgSvcAccessInfo
            }


    type NodeServiceAccessInfo =
        {
            nodeServiceAccessInfo : ServiceAccessInfo
            minUsefulEe : MinUsefulEe
        }


    type WorkerNodeServiceAccessInfo =
        {
            workerNodeServiceAccessInfo : NodeServiceAccessInfo
            nodeInfo : NodeInfo
            workNodeMsgAccessInfo : WorkNodeMsgAccessInfo
            partitionerId : PartitionerId
            isInactive : bool
        }

        member w.workerNodeInfo =
            {
                workerNodeId = w.workNodeMsgAccessInfo.workerNodeId
                nodeInfo = w.nodeInfo
            }
