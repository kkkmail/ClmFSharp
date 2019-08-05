namespace MessagingTestClient

open System.Threading
open MessagingServiceInfo.ServiceInfo
open Messaging.Client
open Messaging.MsgCliCommandLine
open Messaging.ServiceResponse
open MessagingServiceInfo.ServiceInfo
open ClmSys.MessagingData
open ServiceProxy.MessagingClient

module MessagingTestClientTask =

    let runTestClient i h r =
        let logger e = printfn "%A" e

        let d =
            {
                msgAccessInfo = i
                msgResponseHandler = h
                msgClientProxy = ClmMessagingClientProxy.defaultValue
                logger = logger
            }

        let a = ClmMessagingClient d

        while true do
            printfn "Sending message to %A" r

            let m =
                {
                    recipient = r
                    deliveryType = NonGuaranteedDelivery
                    messageData = ClmMesage.Dummmy
                }

            a.sendMessage m

            printfn "Receiving messages."
            let x = a.getMessages()
            printfn "Received %A messages." (x.Length)
            x |> List.map (fun e -> printfn "message: %A" e) |> ignore

            Thread.Sleep 30_000


    type MessagingTestClientTask =
        | RunTestClientTask of MessagingClientAccessInfo * ClmMsgResponseHandler * MessagingClientId

        member task.run() =
            match task with
            | RunTestClientTask (i, h, r) -> runTestClient i h r

        static member tryCreateRunTestClientTask (p : list<MessagingClientRunArgs>) =
            match tryGetClientServiceAccessInfo p None, tryCreateMsgResponseHandler p None, tryGetRecipientId p with
            | Some i, Some h, Some r -> RunTestClientTask (i, h, r) |> Some
            | _ -> None

        static member tryCreate (p : list<MessagingClientRunArgs>) =
            [
                MessagingTestClientTask.tryCreateRunTestClientTask
            ]
            |> List.tryPick (fun e -> e p)
