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
open ClmSys.ContGenErrors

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
            contGenSvcAddress : ContGenServiceAddress
            contGenSvcPort : ContGenServicePort
            minUsefulEe : MinUsefulEe
            msgSvcAddress : MessagingServiceAddress
            msgSvcPort : MessagingServicePort
            partitionerId : PartitionerId
        }
        
        member w.isValid() =
            let r =               
                [
                    w.contGenSvcAddress.value.value <> EmptyString, sprintf "%A is invalid" w.contGenSvcAddress
                    w.contGenSvcPort.value.value > 0, sprintf "%A is invalid" w.contGenSvcPort
                    w.msgSvcAddress.value.value <> EmptyString, sprintf "%A is invalid" w.msgSvcAddress
                    w.msgSvcPort.value.value > 0, sprintf "%A is invalid" w.msgSvcPort
                    w.partitionerId.value.value <> Guid.Empty, sprintf "%A is invalid" w.partitionerId
                ]
                |> List.fold(fun acc r -> combine acc r) (true, EmptyString)
                
            match r with
            | true, _ -> Ok()
            | false, s -> s |> InvalidSettings |> ContGenSettingsErr |> ContGenServiceErr |> Error

