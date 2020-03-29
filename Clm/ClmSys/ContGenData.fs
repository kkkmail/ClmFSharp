namespace ClmSys

open System
open GeneralData
open ContGenPrimitives

module ContGenData =

    type ContGenServiceAccessInfo =
        {
            contGenServiceAddress : ContGenServiceAddress
            contGenServicePort : ContGenServicePort
            contGenServiceName : ContGenServiceName
        }

        member s.serviceName = s.contGenServiceName.value.value
        member s.serviceUrl = getServiceUrlImpl s.contGenServiceAddress.value s.contGenServicePort.value s.serviceName
        member s.wcfServiceName = toValidServiceName s.serviceName
        member s.wcfServiceUrl = getWcfServiceUrlImpl s.contGenServiceAddress.value s.contGenServicePort.value s.wcfServiceName


    type TaskProgress
        with

        member progress.estimateEndTime (started : DateTime) =
            match progress with
            | NotStarted -> None
            | InProgress p -> estimateEndTime p started
            | Completed _ -> Some DateTime.Now
            | Failed _ -> None


    type ContGenServiceInfo =
        {
            contGenServiceAccessInfo : ContGenServiceAccessInfo
            minUsefulEe : MinUsefulEe
        }
