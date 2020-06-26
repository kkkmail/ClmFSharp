namespace ClmSys

open System
open ClmSys.GeneralData
open ClmSys.MessagingData
open ClmSys.WorkerNodePrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.WorkerNodeErrors
open ClmSys.ClmErrors

module WorkerNodeData =

    type WorkerNodeInfo =
        {
            workerNodeId : WorkerNodeId
            workerNodeName : WorkerNodeName
            partitionerId : PartitionerId
            noOfCores : int
            nodePriority : WorkerNodePriority
            isInactive : bool
            lastErrorDateOpt : DateTime option
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

    
    type WorkerNodeSettings =
        {
            svcAddress : WorkerNodeServiceAddress
            svcPort : WorkerNodeServicePort
            name : WorkerNodeName
            noOfCores : int
            msgSvcAddress : MessagingServiceAddress
            msgSvcPort : MessagingServicePort
            msgCliId : WorkerNodeId
            partitioner : PartitionerId
            isInactive : bool
        }
        
        member w.isValid() =
            let r =               
                [
                    w.svcPort.value.value > 0, sprintf "%A is invalid" w.svcPort
                    w.svcAddress.value.value <> EmptyString, sprintf "%A is invalid" w.svcAddress
                    w.name.value <> EmptyString, sprintf "%A is invalid" w.name
                    w.noOfCores >= 0, sprintf "noOfCores: %A is invalid" w.noOfCores
                    w.msgSvcAddress.value.value <> EmptyString, sprintf "%A is invalid" w.msgSvcAddress
                    w.msgSvcPort.value.value > 0, sprintf "%A is invalid" w.msgSvcPort
                    w.msgCliId.value.value <> Guid.Empty, sprintf "%A is invalid" w.msgCliId
                    w.partitioner.value.value <> Guid.Empty, sprintf "%A is invalid" w.partitioner
                ]
                |> List.fold(fun acc r -> combine acc r) (true, EmptyString)
                
            match r with
            | true, _ -> Ok()
            | false, s -> s |> InvalidSettings |> WrkSettingsErr |> WorkerNodeErr |> Error
            