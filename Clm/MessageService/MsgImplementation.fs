namespace MessagingService

open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open MessagingService.SvcCommandLine
open Messaging.Service
open ServiceProxy.MsgServiceProxy
open System.ServiceModel
open ClmSys.Wcf
open ClmSys.MessagingServiceErrors
open ClmSys.ClmErrors

module ServiceImplementation =

    let createServiceImpl (i : MessagingServiceAccessInfo) : MessagingService =
        let d : MessagingServiceData =
            {
                messagingServiceProxy = MessagingServiceProxy.create()
            }

        let service = MessagingService d
        do service.start() |> ignore
        service


    let mutable serviceAccessInfo = getServiceAccessInfo []


    [<ServiceBehavior(IncludeExceptionDetailInFaults = true, InstanceContextMode = InstanceContextMode.Single)>]
    type MessagingWcfService() =
        let a = createServiceImpl serviceAccessInfo
        let toGetVersionError f = f |> GetVersionSvcWcfErr |> GetVersionSvcErr |> MessagingServiceErr
        let toSendMessageError f = f |> MsgWcfErr |> MessageDeliveryErr |> MessagingServiceErr
        let toConfigureServiceError f = f |> CfgSvcWcfErr |> ConfigureServiceErr |> MessagingServiceErr
        let toTryPickMessageError f = f |> TryPeekMsgWcfErr |> TryPeekMessageErr |> MessagingServiceErr
        let toTryDeleteFromServerError f = f |> TryDeleteMsgWcfErr |> TryDeleteFromServerErr |> MessagingServiceErr
        let toGetStateError f = f |> GetStateWcfErr |> GetStateErr |> MessagingServiceErr

        interface IMessagingWcfService with
            member __.getVersion b = tryReply a.getVersion toGetVersionError b
            member __.sendMessage b = tryReply a.sendMessage toSendMessageError b
            member __.configureService b = tryReply a.configureService toConfigureServiceError b
            member __.tryPeekMessage b = tryReply a.tryPeekMessage toTryPickMessageError b
            member __.tryDeleteFromServer b = tryReply a.tryDeleteFromServer toTryDeleteFromServerError b
            member __.getState b = tryReply a.getState toGetStateError b
