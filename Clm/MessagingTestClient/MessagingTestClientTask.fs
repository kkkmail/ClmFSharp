namespace MessagingTestClient

open System
open System.Threading
open ClmSys.GeneralPrimitives
open MessagingServiceInfo.ServiceInfo
open Messaging.Client
open Messaging.MsgCliCommandLine
open Messaging.ServiceResponse
open ClmSys.MessagingData
open ServiceProxy.MsgServiceProxy
open ServiceProxy.MsgProcessorProxy
open ClmSys.MessagingPrimitives

module MessagingTestClientTask =

    let runTestClient i h r =

        let j =
            {
                messagingClientName = MessagingClientName ("TestClient_" + i.msgClientId.value.ToString())
                storageType = ":memory:" |> SqliteConnectionString |> SqliteDatabase
            }

        let d =
            {
                msgAccessInfo = i
                messagingService = h
                msgClientProxy = MessagingClientProxy.create j i.msgClientId
            }

        let a = MessagingClient d
        do a.start() |> ignore

        let tryProcessMessage = onTryProcessMessage a.messageProcessorProxy

        while true do
//            printfn "Getting version number for: %A" r
//            let version = a.getVersion()
//            printfn "Version number: %A" version

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

            a.sendMessage m |> ignore
            printfn "Checking messages."


            let checkMessage() =
                match tryProcessMessage () (fun _ m -> m) with
                | ProcessedSuccessfully m -> printfn "    Received message: %A" m
                | ProcessedWithError (m, e) -> printfn "    Received message: %A with error e: %A" m e
                | ProcessedWithFailedToRemove (m, e) -> printfn "    Received message: %A with error e: %A" m e
                | FailedToProcess e -> printfn "    Error e: %A" e
                | NothingToDo -> ignore()
                | BusyProcessing -> ignore()

            let t = [for _ in 1..20 -> ()] |> List.map checkMessage
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
