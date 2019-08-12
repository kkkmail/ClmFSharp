﻿namespace ClmSys

open ClmSys.GeneralData
open MessagingData

module WorkerNodeData =

    type WorkerNodeInfo =
        {
            workerNodeId : WorkerNodeId
            noOfCores : int
        }


    type WorkerNodeServiceAccessInfo =
        {
            msgCliAccessInfo : MessagingClientAccessInfo
            noOfCores : int
            partitionerId : PartitionerId
            storageId : StorageId
            wrkSvcAccessInfo : ServiceAccessInfo
        }

        member w.workerNodeInfo =
            {
                workerNodeId = w.msgCliAccessInfo.msgClientId |> WorkerNodeId
                noOfCores = w.noOfCores
            }
