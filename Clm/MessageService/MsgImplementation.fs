﻿namespace MessagingService

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

module ServiceImplementation =

    let mutable serviceAccessInfo = getServiceAccessInfo []


    let createServiceImpl (i : MessagingServiceAccessInfo) : MessagingService =
        let d : MessagingServiceData =
            {
                messagingServiceProxy = MessagingServiceProxy.create msgSvcConnectionString
            }

        let service = MessagingService d
        service


    [<ServiceBehavior(IncludeExceptionDetailInFaults = true, InstanceContextMode = InstanceContextMode.Single)>]
    type MessagingWcfService() =
        let a = createServiceImpl serviceAccessInfo
        let toGetVersionError f = f |> GetVersionSvcWcfErr |> GetVersionSvcErr |> MessagingServiceErr
        let toSendMessageError f = f |> MsgWcfErr |> MessageDeliveryErr |> MessagingServiceErr
        let toTryPickMessageError f = f |> TryPeekMsgWcfErr |> TryPeekMessageErr |> MessagingServiceErr
        let toTryDeleteFromServerError f = f |> TryDeleteMsgWcfErr |> TryDeleteFromServerErr |> MessagingServiceErr

        interface IMessagingWcfService with
            member _.getVersion b = tryReply a.getVersion toGetVersionError b
            member _.sendMessage b = tryReply a.sendMessage toSendMessageError b
            member _.tryPeekMessage b = tryReply a.tryPeekMessage toTryPickMessageError b
            member _.tryDeleteFromServer b = tryReply a.tryDeleteFromServer toTryDeleteFromServerError b
