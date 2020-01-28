﻿namespace ClmSys

open GeneralData
open MessagingPrimitives
open MessagingData
open WorkerNodePrimitives
open PartitionerPrimitives

module WorkerNodeData =

    type WorkerNodeInfo =
        {
            workerNodeId : WorkerNodeId
            noOfCores : int
            nodePriority : WorkerNodePriority
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
            noOfCores : int
            isInactive : bool
            workNodeMsgAccessInfo : WorkNodeMsgAccessInfo
            partitionerId : PartitionerId
            nodePriority : WorkerNodePriority
        }

        member w.workerNodeInfo =
            {
                workerNodeId = w.workNodeMsgAccessInfo.workerNodeId
                noOfCores = w.noOfCores
                nodePriority = w.nodePriority
            }
