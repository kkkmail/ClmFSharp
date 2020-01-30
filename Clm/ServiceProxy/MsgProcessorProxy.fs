﻿namespace ServiceProxy

open MessagingServiceInfo.ServiceInfo
open ClmSys.ClmErrors
open ClmSys.MessagingErrors
open ClmSys.MessagingPrimitives

module MsgProcessorProxy =

    type TryRemoveReceivedMessageResult =
        | RemovedSucessfully
        | RemovedWithError of ClmError
        | FailedToRemove of ClmError


    type MessageProcessorResult<'T> =
        | ProcessedSucessfully of 'T
        | ProcessedWithError of ('T * ClmError)
        | ProcessedWithFailedToRemove of ('T * ClmError)
        | FailedToProcess of ClmError
        | NothingToDo
        | BusyProcessing


    type MessageProcessorProxy =
        {
            tryPeekReceivedMessage : unit -> Message option
            tryRemoveReceivedMessage : MessageId -> TryRemoveReceivedMessageResult
            sendMessage : MessageInfo -> UnitResult
        }


    type OnProcessMessageType<'S> = 'S -> Message -> StateWithResult<'S>
    type MessageResult<'S> = MessageProcessorResult<'S * UnitResult>


    type OnGetMessagesProxy<'S> =
        {
            tryProcessMessage : 'S -> OnProcessMessageType<'S> -> MessageResult<'S>
            onProcessMessage : 'S -> Message -> StateWithResult<'S>
            maxMessages : list<unit>
            onError : OnGetMessagesError -> ClmError
        }


    let onGetMessages<'S> (proxy : OnGetMessagesProxy<'S>) (s : 'S) =
        let addError f e = ((proxy.onError f) + e) |> Error
        let toError e = e |> proxy.onError |> Error

        let rec doFold x acc =
            match x with
            | [] -> acc, Ok()
            | _ :: t ->
                match proxy.tryProcessMessage acc proxy.onProcessMessage with
                | ProcessedSucessfully (g, u) ->
                    match u with
                    | Ok() -> doFold t g
                    | Error e -> g, addError ProcessedSucessfullyWithInnerErr e
                | ProcessedWithError ((g, u), e) -> g, (addError ProcessedWithErr e, u) ||> combineUnitResults
                | ProcessedWithFailedToRemove((g, u), e) -> g, (addError ProcessedWithFailedToRemoveErr e, u) ||> combineUnitResults
                | FailedToProcess e -> acc, addError FailedToProcessErr e
                | NothingToDo -> acc, Ok()
                | BusyProcessing -> acc, toError BusyProcessingErr

        let w, result = doFold proxy.maxMessages s
        w, result
