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

        member private s.serviceName = s.messagingServiceName.value.value
        member s.wcfServiceName = toValidServiceName s.serviceName
        member s.wcfServiceUrl = getWcfServiceUrlImpl s.messagingServiceAddress.value s.messagingServicePort.value s.wcfServiceName


    type MessagingClientAccessInfo =
        {
            msgClientId : MessagingClientId
            msgSvcAccessInfo : MessagingServiceAccessInfo
        }
