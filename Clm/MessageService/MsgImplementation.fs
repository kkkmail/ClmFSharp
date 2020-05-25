namespace MessagingService

open System.ServiceModel
open ClmSys.MessagingData
open MessagingServiceInfo.ServiceInfo
open MessagingService.SvcCommandLine
open Messaging.Service
open ServiceProxy.MsgServiceProxy
open ClmSys.Wcf
open ClmSys.MessagingServiceErrors
open ClmSys.ClmErrors
open DbData.Configuration
open ClmSys

module ServiceImplementation =

    let mutable serviceAccessInfo = getServiceAccessInfo []


    let private createMessagingService (i : MessagingServiceAccessInfo) : MessagingService =
        let d : MessagingServiceData =
            {
                messagingServiceProxy = MessagingServiceProxy.create msgSvcConnectionString
            }

        let service = MessagingService d
        service


    let private messagingService = new Lazy<ClmResult<MessagingService>>(fun () -> createMessagingService serviceAccessInfo |> Ok)


//    [<ServiceBehavior(IncludeExceptionDetailInFaults = true, InstanceContextMode = InstanceContextMode.Single)>]
    [<ServiceBehavior(IncludeExceptionDetailInFaults = true, InstanceContextMode = InstanceContextMode.PerSession)>]
    type MessagingWcfService() =
        let toGetVersionError f = f |> GetVersionSvcWcfErr |> GetVersionSvcErr |> MessagingServiceErr
        let toSendMessageError f = f |> MsgWcfErr |> MessageDeliveryErr |> MessagingServiceErr
        let toTryPickMessageError f = f |> TryPeekMsgWcfErr |> TryPeekMessageErr |> MessagingServiceErr
        let toTryDeleteFromServerError f = f |> TryDeleteMsgWcfErr |> TryDeleteFromServerErr |> MessagingServiceErr

        let getVersion() = messagingService.Value |> Rop.bind (fun e -> e.getVersion())
        let sendMessage b = messagingService.Value |> Rop.bind (fun e -> e.sendMessage b)
        let tryPeekMessage b = messagingService.Value |> Rop.bind (fun e -> e.tryPeekMessage b)
        let tryDeleteFromServer b = messagingService.Value |> Rop.bind (fun e -> e.tryDeleteFromServer b)

        interface IMessagingWcfService with
            member _.getVersion b = tryReply getVersion toGetVersionError b
            member _.sendMessage b = tryReply sendMessage toSendMessageError b
            member _.tryPeekMessage b = tryReply tryPeekMessage toTryPickMessageError b
            member _.tryDeleteFromServer b = tryReply tryDeleteFromServer toTryDeleteFromServerError b
