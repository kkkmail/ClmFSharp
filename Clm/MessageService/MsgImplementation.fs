namespace MessagingService

open System
open ClmSys.Logging
open ClmSys.MessagingData
open ClmSys.GeneralErrors
open MessagingServiceInfo.ServiceInfo
open MessagingService.SvcCommandLine
open Messaging.Service
open ServiceProxy.MsgServiceProxy
open ClmSys.Rop
open ClmSys.GeneralData
open System.ServiceModel
open ClmSys.Wcf
open ClmSys.MessagingServiceErrors
open ClmSys.ClmErrors

module ServiceImplementation =

    let private toMessagingServiceErr g f = f |> g |> MessagingServiceErr
    let private toError g f = f |> g |> MessagingServiceErr |> Error
    let private addError g f e = ((f |> g |> MessagingServiceErr) + e) |> Error

    let createServiceImpl (i : MessagingServiceAccessInfo) : MessagingService =
        let d : MessagingServiceData =
            {
                messagingServiceProxy = MessagingServiceProxy.create()
                //logger = Logger.log4net
            }

        let service = MessagingService d
        do service.start() |> ignore
        service


    let mutable serviceAccessInfo = getServiceAccessInfo []


    type MessagingRemoteService () =
        inherit MarshalByRefObject()

        let a = createServiceImpl serviceAccessInfo

        let initService () = ()
        do initService ()

        interface IMessagingService with
            member __.getVersion() = a.getVersion()
            member __.sendMessage m = a.sendMessage m
            member __.configureService x = a.configureService x
            member __.tryPeekMessage n = a.tryPeekMessage n
            member __.tryDeleteFromServer x = a.tryDeleteFromServer x
            member __.getState() = a.getState()


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
