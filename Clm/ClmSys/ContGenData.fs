namespace ClmSys

open System
open System.ServiceModel

open ClmSys
open GeneralPrimitives
open GeneralData
open ContGenPrimitives
open ClmSys.PartitionerPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.ClmErrors
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


    /// Number of minutes for worker node errors to expire before the node can be again included in work distribution.
    let defaultLastAllowedNodeErr = LastAllowedNodeErr 60<minute>


    type ContGenSettings =
        {
            contGenSvcAddress : ContGenServiceAddress
            contGenSvcPort : ContGenServicePort
            minUsefulEe : MinUsefulEe
            msgSvcAddress : MessagingServiceAddress
            msgSvcPort : MessagingServicePort
            partitionerId : PartitionerId
            lastAllowedNodeErr : LastAllowedNodeErr
        }

        member w.isValid() =
            let r =
                [
                    w.contGenSvcAddress.value.value <> EmptyString, sprintf "%A is invalid" w.contGenSvcAddress
                    w.contGenSvcPort.value.value > 0, sprintf "%A is invalid" w.contGenSvcPort
                    w.msgSvcAddress.value.value <> EmptyString, sprintf "%A is invalid" w.msgSvcAddress
                    w.msgSvcPort.value.value > 0, sprintf "%A is invalid" w.msgSvcPort
                    w.partitionerId.value.value <> Guid.Empty, sprintf "%A is invalid" w.partitionerId
                    w.lastAllowedNodeErr.value > 0<minute>, sprintf "%A is invalid" w.lastAllowedNodeErr
                ]
                |> List.fold(fun acc r -> combine acc r) (true, EmptyString)

            match r with
            | true, _ -> Ok()
            | false, s -> s |> InvalidSettings |> ContGenSettingsErr |> ContGenServiceErr |> Error

