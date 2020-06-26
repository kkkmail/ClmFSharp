namespace ClmSys

open GeneralData
open ContGenPrimitives
open System.ServiceModel

open System
open ClmSys.GeneralData
open ClmSys.MessagingData
open ClmSys.WorkerNodePrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.WorkerNodeErrors
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives

module ContGenData =

    type ContGenServiceAccessInfo =
        {
            contGenServiceAddress : ContGenServiceAddress
            contGenServicePort : ContGenServicePort
            contGenServiceName : ContGenServiceName
        }

        member private s.serviceName = s.contGenServiceName.value.value
        member s.wcfServiceName = toValidServiceName s.serviceName
        member s.wcfServiceUrl = getWcfServiceUrlImpl s.contGenServiceAddress.value s.contGenServicePort.value s.wcfServiceName


    type ContGenWcfSvcShutDownInfo =
        {
            contGenServiceHost : ServiceHost
        }
        
    type ContGenSettings =
        {
            svcAddress : ContGenServiceAddress
            svcPort : ContGenServicePort
            minUsefulEe : MinUsefulEe
            contGenServiceName : ContGenServiceName
            msgSvcAddress : MessagingServiceAddress
            msgSvcPort : MessagingServicePort
//            msgCliId : WorkerNodeId
//            partitioner : PartitionerId
        }
        
        member w.isValid() =
            let r =               
                [
                    w.svcPort.value.value > 0, sprintf "%A is invalid" w.svcPort
                    w.svcAddress.value.value <> EmptyString, sprintf "%A is invalid" w.svcAddress
                    w.contGenServiceName.value.value <> EmptyString, sprintf "%A is invalid" w.contGenServiceName
                    w.msgSvcAddress.value.value <> EmptyString, sprintf "%A is invalid" w.msgSvcAddress
                    w.msgSvcPort.value.value > 0, sprintf "%A is invalid" w.msgSvcPort
//                    w.msgCliId.value.value <> Guid.Empty, sprintf "%A is invalid" w.msgCliId
//                    w.partitioner.value.value <> Guid.Empty, sprintf "%A is invalid" w.partitioner
                ]
                |> List.fold(fun acc r -> combine acc r) (true, EmptyString)
                
            match r with
            | true, _ -> Ok()
            | false, s -> s |> InvalidSettings |> WrkSettingsErr |> WorkerNodeErr |> Error

