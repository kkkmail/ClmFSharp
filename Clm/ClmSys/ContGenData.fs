namespace ClmSys

open GeneralData
open ContGenPrimitives

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
