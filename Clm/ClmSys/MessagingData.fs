namespace ClmSys

open GeneralData
open MessagingPrimitives

module MessagingData =

    type MessagingServiceAccessInfo =
        {
            messagingServiceAddress : MessagingServiceAddress
            messagingServicePort : MessagingServicePort
            messagingServiceName : MessagingServiceName
        }

        member s.serviceName = s.messagingServiceName.value.value
        member s.serviceUrl = getServiceUrlImpl s.messagingServiceAddress.value s.messagingServicePort.value s.serviceName
        member s.wcfServiceName = toValidServiceName s.serviceName
        member s.wcfServiceUrl = getWcfServiceUrlImpl s.messagingServiceAddress.value s.messagingServicePort.value s.wcfServiceName


    type MessagingClientAccessInfo =
        {
            msgClientId : MessagingClientId
            msgSvcAccessInfo : MessagingServiceAccessInfo
        }
