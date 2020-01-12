namespace ServiceProxy

open MessagingServiceInfo.ServiceInfo
open ClmSys.GeneralErrors

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
            //sendMessage : MessageInfo -> UnitResult
            tryPeekReceivedMessage : unit -> Message option
            tryRemoveReceivedMessage : MessageId -> TryRemoveReceivedMessageResult
        }
