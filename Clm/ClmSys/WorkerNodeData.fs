namespace ClmSys

open GeneralData
open MessagingPrimitives
open MessagingData
open WorkerNodePrimitives
open PartitionerPrimitives
open ContGenPrimitives

module WorkerNodeData =

    type WorkerNodeInfo =
        {
            workerNodeId : WorkerNodeId
            workerNodeName : WorkerNodeName
            noOfCores : int
            nodePriority : WorkerNodePriority
        }


    type WorkerNodeRequestInfo =
        {
            workerNodeId : WorkerNodeId
            requestedWorkItems : int
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


    type WorkerNodeServiceAccessInfo =
        {
            workerNodeServiceAccessInfo : ServiceAccessInfo
            workerNodeName : WorkerNodeName
            noOfCores : int
            isInactive : bool
            workNodeMsgAccessInfo : WorkNodeMsgAccessInfo
            partitionerId : PartitionerId
            nodePriority : WorkerNodePriority
        }

        member w.workerNodeInfo =
            {
                workerNodeId = w.workNodeMsgAccessInfo.workerNodeId
                workerNodeName = w.workerNodeName
                noOfCores = w.noOfCores
                nodePriority = w.nodePriority
            }


    type WrkNodeServiceAccessInfo =
        {
            wrkNodeServiceAccessInfo : ServiceAccessInfo
            minUsefulEe : MinUsefulEe
        }
