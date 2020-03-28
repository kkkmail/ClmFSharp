namespace ClmSys

open System
open GeneralPrimitives
open ClmSys.VersionInfo

module MessagingPrimitives =

    type MessagingServiceAddress =
        | MessagingServiceAddress of ServiceAddress

        member this.value = let (MessagingServiceAddress v) = this in v
        static member defaultValue = DefaultMessagingServerAddress |> ServiceAddress |> MessagingServiceAddress


    type MessagingServicePort =
        | MessagingServicePort of ServicePort

        member this.value = let (MessagingServicePort v) = this in v
        static member defaultValue = DefaultMessagingServerPort |> ServicePort |> MessagingServicePort


    type MessagingServiceName =
        | MessagingServiceName of ServiceName

        member this.value = let (MessagingServiceName v) = this in v


    let messagingServiceName =
        "MessagingService" + " - " + versionNumberValue.value
        |> ServiceName
        |> MessagingServiceName


    type MessagingClientName =
        | MessagingClientName of string

        member this.value = let (MessagingClientName v) = this in v


    type MessagingClientId =
        | MessagingClientId of Guid

        member this.value = let (MessagingClientId v) = this in v
        static member create() = Guid.NewGuid() |> MessagingClientId


    type MessageId =
        | MessageId of Guid

        member this.value = let (MessageId v) = this in v
        static member create() = Guid.NewGuid() |> MessageId
