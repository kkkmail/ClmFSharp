namespace MessagingTestClient

open System
open System.Threading
open MessagingServiceInfo.ServiceInfo
open Messaging.Client
open Messaging.MsgCliCommandLine
open Messaging.ServiceResponse
open ClmSys.MessagingData
open ClmSys.Logging
open ServiceProxy.MsgServiceProxy
open ClmSys.GeneralData

module MessagingTestClientTask =

    let runTestClient i h r =

        let d =
            {
                msgAccessInfo = i
                messagingService = h
                msgClientProxy = MessagingClientProxy { messagingClientName = MessagingClientName ("TestClient_" + i.msgClientId.value.ToString()) }
                logger = logger
            }

        let a = MessagingClient d
        do a.start()

        while true do
            printfn "Getting version number for: %A" r
            let version = a.getVersion()
            printfn "Version number: %A" version

            printfn "Sending message to %A" r

            let m =
                {
                    recipientInfo =
                        {
                            recipient = r
                            deliveryType = GuaranteedDelivery
                        }

                    messageData = sprintf "Message sent at %A." DateTime.Now |> TextData
                }

            a.sendMessage m
            printfn "Checking messages."

            let checkMessage() =
                async {
                    match! a.tryProcessMessage () (fun _ m -> m) with
                    | Some m ->
                        printfn "    Received message: %A" m
                    | None -> ignore()
                }

            let t = [for _ in 1..20 -> ()] |> List.mapAsync checkMessage
            t |> Async.RunSynchronously |> ignore
            Thread.Sleep 30_000


    type MessagingTestClientTask =
        | RunTestClientTask of MessagingClientAccessInfo * MsgResponseHandler * MessagingClientId

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
