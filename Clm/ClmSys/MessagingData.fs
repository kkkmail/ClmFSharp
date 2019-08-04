namespace ClmSys

open System
open ClmSys.GeneralData

module MessagingData =

    type MessagingClientName =
        | MessagingClientName of string

        member this.value = let (MessagingClientName v) = this in v


    type MessagingClientId =
        | MessagingClientId of Guid

        member this.value = let (MessagingClientId v) = this in v
        static member create() = Guid.NewGuid() |> MessagingClientId


    type MessagingClientAccessInfo =
        {
            msgClientId : MessagingClientId
            msgSvcAccessInfo : ServiceAccessInfo
        }


    type MessagingServiceAccessInfo =
        {
            messagingServiceAccessInfo : ServiceAccessInfo
        }

