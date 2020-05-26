namespace Messaging

open System
open ClmSys.VersionInfo
open MessagingServiceInfo.ServiceInfo
open ServiceProxy.MsgServiceProxy
open ClmSys.ClmErrors
open ClmSys.MessagingPrimitives

module Service =

    type MessagingServiceData =
        {
            messagingServiceProxy : MessagingServiceProxy
            expirationTime : TimeSpan
        }
        static member defaultExpirationTime = TimeSpan.FromHours 6.0

    type MessagingService(d : MessagingServiceData) =
        let proxy = d.messagingServiceProxy
        member _.getVersion() : ClmResult<MessagingDataVersion> = Ok messagingDataVersion

        member _.sendMessage (m : Message) : UnitResult =
            let result = proxy.saveMessage m

            match result with
            | Ok() -> ignore()
            | Error e -> printfn "ERROR - MessagingService.sendMessage failed for messageId: %A with error: %A" m.messageDataInfo.messageId e
            result

        member _.tryPeekMessage (n : MessagingClientId) : ClmResult<Message option> = proxy.tryPickMessage n
        member _.tryDeleteFromServer (n : MessagingClientId, m : MessageId) : UnitResult = proxy.deleteMessage m
        member _.removeExpiredMessages() : UnitResult = proxy.deleteExpiredMessages d.expirationTime
