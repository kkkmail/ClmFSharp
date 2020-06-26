namespace ClmSys

open GeneralData
open MessagingPrimitives
open ClmSys.ClmErrors
open ClmSys.MessagingServiceErrors

module MessagingData =

    [<Literal>]
    let MsgDatabase = "MsgClient.db"


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
        
        
    type MsgSettings =
        {
            msgSvcAddress : MessagingServiceAddress
            msgSvcPort : MessagingServicePort
        }
        
        member w.isValid() =
            let r =               
                [
                    w.msgSvcAddress.value.value <> EmptyString, sprintf "%A is invalid" w.msgSvcAddress
                    w.msgSvcPort.value.value > 0, sprintf "%A is invalid" w.msgSvcPort
                ]
                |> List.fold(fun acc r -> combine acc r) (true, EmptyString)
                
            match r with
            | true, _ -> Ok()
            | false, s -> s |> InvalidSettings |> MsgSettingsErr |> MessagingServiceErr |> Error
