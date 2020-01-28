namespace ClmSys

open GeneralData
open MessagingPrimitives

module MessagingData =

    type MessagingClientAccessInfo =
        {
            msgClientId : MessagingClientId
            msgSvcAccessInfo : ServiceAccessInfo
        }


    type MessagingServiceAccessInfo =
        {
            messagingServiceAccessInfo : ServiceAccessInfo
        }
