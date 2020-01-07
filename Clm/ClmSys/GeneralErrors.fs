namespace ClmSys

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

open ClmSys.VersionInfo


/// Collection of general errors & related functionality.
module GeneralErrors =

    type ErrorId =
        | ErrorId of Guid


    type TraceInfo =
        {
            memberName : string
            sourcePath : string
            sourceLine : int
        }


    type Tracer() =
        member _.doTrace([<CallerMemberName; Optional; DefaultParameterValue("")>] memberName: string,
                          [<CallerFilePath; Optional; DefaultParameterValue("")>] path: string,
                          [<CallerLineNumber; Optional; DefaultParameterValue(0)>] line: int) =

            {
                memberName = memberName
                sourcePath = path
                sourceLine = line
            }


    let tracer = new Tracer()


    type FileError =
        | GeneralFileException of exn
        | GetFolderNameException of exn
        | GetFileNameException of exn
        | FileNotFound of string
        | ReadFileException of exn
        | WriteFileException of exn
        | DeleteFileException of exn
        | GetObjectIdsException of exn
        | CreateChartsException of exn
        | SaveChartsException of exn


    type JsonParseError =
        | InvalidStructure of string


    type SerializationError =
        | SerializationException of exn
        | DeserializationException of exn


    type WcfError =
        | WcfException of exn
        | WcfSerializationError of SerializationError


    type DbError =
        | DbException of exn
        | LoadModelDataError of Guid
        | SaveResultDataError of Guid
        | LoadResultDataError of Guid
        | LoadClmDefaultValueError of Int64
        | UpsertClmDefaultValueError of Int64
        | LoadClmTaskByDefaultError of Int64
        | LoadClmTaskError of Guid
        | UpdateClmTaskError of Guid
        | UpdateModelDataError of Guid
        | ClmTaskTryCreatError of Guid
        | DeleteRunQueueEntryError of Guid


    type ProcessStartedError =
        | AlreadyCompleted
        | FailedToStart of exn


    type VersionMismatchInfo =
        {
            localVersion : int
            remoteVersion : int
        }


    type GetVersionError =
        | GetVersionWcfError of WcfError
        | VersionMismatchError of VersionMismatchInfo


    type MessageDeliveryError =
        | DataVersionMismatch of MessagingDataVersion
        | MsgWcfError of WcfError
        | ServerIsShuttingDown


    type ConfigureServiceError =
        | CfgSvcWcfError of WcfError


    type TryPeekMessageError =
        | TryPeekMsgWcfError of WcfError


    type TryDeleteFromServerError =
        | TryDeleteMsgWcfError of WcfError


    type GetStateError =
        | GetStateWcfError of WcfError

    type MessageNotFoundError =
        | MessageNotFoundError of Guid


    type MessagingServiceError =
        | GetVersionErr of GetVersionError
        | MessageDeliveryErr of MessageDeliveryError
        | ConfigureServiceErr of ConfigureServiceError
        | TryPeekMessageErr of TryPeekMessageError
        | TryDeleteFromServerErr of TryDeleteFromServerError
        | GetStateErr of GetStateError


    type MessagingClientError =
        | MessageNotFoundErr of MessageNotFoundError


    /// All errors known in the system.
    type ClmError =
        | AggregateErr of List<ClmError>
        | UnhandledExn of exn
        | UnknownErr of string
        | FileErr of FileError
        | SerializationErr of SerializationError
        | WcfErr of WcfError
        | DbErr of DbError
        | ProcessStartedErr of ProcessStartedError
        | MessagingServiceErr of MessagingServiceError
        | MessagingClientErr of MessagingClientError

        static member (+) (a, b) =
            match a, b with
            | AggregateErr x, AggregateErr y -> AggregateErr (x @ y)
            | AggregateErr x, _ -> AggregateErr (x @ [b])
            | _, AggregateErr y -> AggregateErr (a :: y)
            | _ -> AggregateErr [ a; b ]


    let foldErrors (a : list<ClmError>) =
        match a with
        | [] -> None
        | h :: t -> t |> List.fold (fun acc r -> r + acc) h |> Some


    type UnitResult = Result<unit, ClmError>
    type ClmResult<'T> = Result<'T, ClmError>
    type ListResult<'T> = Result<list<Result<'T, ClmError>>, ClmError>


    type ClmErrorInfo =
        {
            errorId : ErrorId
            traceInfo : TraceInfo
            error : ClmError
        }
