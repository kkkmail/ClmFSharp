namespace MessagingTestClient

open System
open Argu
open FSharp.Configuration
open ClmSys.MessagingData
open Messaging.ServiceResponse
open ClmSys.GeneralPrimitives
open ClmSys.MessagingPrimitives
open ClmSys.GeneralData

module MessagingTestClientCommandLine =

    type MessagingTestClientAppSettings = AppSettings<"app.config">


    type MessagingTestClientSettings =
        {
            msgSvcAddress : MessagingServiceAddress
            msgSvcPort : MessagingServicePort
            testClientId : MessagingClientId
        }

        member w.saveSettings() =
            MessagingTestClientAppSettings.MsgSvcAddress <- w.msgSvcAddress.value.value
            MessagingTestClientAppSettings.MsgSvcPort <- w.msgSvcPort.value.value
            MessagingTestClientAppSettings.TestClientId <- w.testClientId.value


    [<CliPrefix(CliPrefix.Dash)>]
    type MessagingClientRunArgs =
        | [<Unique>] [<AltCommandLine("-address")>] MsgCliSvcAddress of string
        | [<Unique>] [<AltCommandLine("-port")>] MsgCliSvcPort of int
        | [<Unique>] [<AltCommandLine("-save")>] MsgCliSaveSettings
        | [<Unique>] [<AltCommandLine("-id")>] MsgCliId of Guid

        /// For debugging.
        | [<Unique>] [<AltCommandLine("-rcp")>] MsgRcpId of Guid

    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | MsgCliSvcAddress _ -> "messaging server ip address / name."
                | MsgCliSvcPort _ -> "messaging server port."
                | MsgCliSaveSettings -> "saves settings to the Registry."
                | MsgCliId _ -> "id of the client - it is like an \"email\" address of a client."

                /// For debugging, comment when not needed.
                | MsgRcpId _ -> "id of message recipient."


    let tryGetMsgServiceAddress p = p |> List.tryPick (fun e -> match e with | MsgCliSvcAddress s -> s |> ServiceAddress |> MessagingServiceAddress |> Some | _ -> None)
    let tryGetMsgServicePort p = p |> List.tryPick (fun e -> match e with | MsgCliSvcPort p -> p |> ServicePort |> MessagingServicePort |> Some | _ -> None)
    let tryGetSaveSettings p = p |> List.tryPick (fun e -> match e with | MsgCliSaveSettings -> Some () | _ -> None)
    let tryGetClientId p = p |> List.tryPick (fun e -> match e with | MsgCliId p -> p |> MessagingClientId |> Some | _ -> None)
    let tryGetRecipientId p = p |> List.tryPick (fun e -> match e with | MsgRcpId p -> p |> MessagingClientId |> Some | _ -> None)

    let getMsgServiceAddress (w: MessagingTestClientSettings) p = tryGetMsgServiceAddress p |> Option.defaultValue w.msgSvcAddress
    let getMsgServicePort (w: MessagingTestClientSettings) p = tryGetMsgServicePort p |> Option.defaultValue w.msgSvcPort


    let saveSettings w =
        MessagingTestClientAppSettings.MsgSvcAddress <- w.msgSvcAccessInfo.messagingServiceAddress.value.value
        MessagingTestClientAppSettings.MsgSvcPort <- w.msgSvcAccessInfo.messagingServicePort.value.value
        MessagingTestClientAppSettings.TestClientId <- w.msgClientId.value


    let loadSettings p =
        let w =
            {
                msgSvcAddress =
                    match MessagingTestClientAppSettings.MsgSvcAddress with
                    | EmptyString -> MessagingServiceAddress.defaultValue
                    | s -> s |> ServiceAddress |> MessagingServiceAddress

                msgSvcPort =
                    match MessagingTestClientAppSettings.MsgSvcPort with
                    | n  when n > 0 -> n |> ServicePort |> MessagingServicePort
                    | _ -> MessagingServicePort.defaultValue

                testClientId = MessagingTestClientAppSettings.TestClientId |> MessagingClientId
            }

        match tryGetClientId p with
        | Some c ->
            let w =
                {
                    msgClientId = c

                    msgSvcAccessInfo =
                        {
                            messagingServiceAddress = getMsgServiceAddress w p
                            messagingServicePort = getMsgServicePort w p
                            messagingServiceName = messagingServiceName
                        }
                }
            saveSettings w
            Some w
        | None -> None


    let tryCreateMsgResponseHandler p =
        match loadSettings p with
        | Some i -> i |> MsgResponseHandler |> Some
        | None -> None
