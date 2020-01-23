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


    type ServiceInstallerError =
        | InstallServiceError of exn
        | UninstallServiceError of exn
        | StartServiceError of exn
        | StopServiceError of exn


    type RegistryErrorInfo =
        {
            version : string
            client : string
            data : string
        }


    type RegistryError =
        | CreateRegistrySubKeyError of string * exn
        | SetRegistryValueError of string * exn
        | GetRegistryValueError of string * exn
        | GetMessagingClientPortError of RegistryErrorInfo
        | GetMessagingClientIdError of RegistryErrorInfo
        | GetPartitionerMessagingClientIdError of RegistryErrorInfo
        | GetUsePartitionerError of RegistryErrorInfo
        | GetNumberOfCoresError of RegistryErrorInfo
        | GetWrkInactiveError of RegistryErrorInfo
        | GetContGenServicePortError of RegistryErrorInfo
        | GetContGenMinUsefulEeError of RegistryErrorInfo


    type ProcessStartedError =
        | FailedToStart of exn


    type VersionMismatchInfo =
        {
            localVersion : int
            remoteVersion : int
        }


    type ClmEventHandlerError =
        | UnhandledException of Guid * exn
        | StillRunningError of Guid


    type GetVersionError =
        | GetVersionWcfError of WcfError
        | VersionMismatchError of VersionMismatchInfo


    type MessageDeliveryError =
        | ServiceNotStarted
        | ServerIsShuttingDown
        | DataVersionMismatch of MessagingDataVersion
        | MsgWcfError of WcfError


    type ConfigureServiceError =
        | CfgSvcWcfError of WcfError


    type TryPeekMessageError =
        | TryPeekMsgWcfError of WcfError
        | UnableToLoadMessageError of (Guid * Guid)


    type TryDeleteFromServerError =
        | TryDeleteMsgWcfError of WcfError
        | CannotFindClientError of Guid
        | UnableToDeleteMessageError of (Guid * Guid)


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
        | TryProcessMessageErr of exn


    type OnSaveResultError =
        | LoadResultDataErr of Guid
        | SendResultMessageError of (Guid * Guid)
        | DeleteResultDataError of Guid


    type OnSaveChartsError =
        | LoadChartInfoError of Guid
        | SendChartMessageError of (Guid * Guid)
        | DeleteChartError of exn
        | DeleteChartInfoError of Guid


    type OnUpdateProgressError =
        | UnableToFindMappingError of int


    type OnRunModelError =
        | CannotSaveWorkerNodeRunModelData of string
        | CannotRunModel of string


    type OnProcessMessageError =
        | CannotSaveModelData
        | ModelAlreadyRunning of Guid
        | InvalidMessage of string


    type OnGetMessagesError =
        | ProcessedSucessfullyWithInnerError
        | ProcessedWithErr
        | ProcessedWithFailedToRemoveError
        | FailedToProcessError
        | BusyProcessingError


    type WorkerNodeError =
        | OnSaveResultErr of OnSaveResultError
        | OnSaveChartsErr of OnSaveChartsError
        | OnUpdateProgressErr of OnUpdateProgressError
        | OnRunModelErr of OnRunModelError
        | OnProcessMessageErr of OnProcessMessageError
        | OnGetMessagesErr of OnGetMessagesError


    type WorkerNodeServiceError =
        | UnableToStartMessagingClientError
        | UnableToCreateWorkerNodeServiceError
        | ServiceUnavailable
        | UpdateLocalProgressError of string
        | ConfigureServiceError of string
        | MonitorServiceError of string


    /// All errors known in the system.
    type ClmError =
        | AggregateErr of ClmError * List<ClmError>
        | UnhandledExn of exn
        | UnknownErr of string
        | ClmEventHandlerErr of ClmEventHandlerError
        | ServiceInstallerErr of ServiceInstallerError
        | RegistryErr of RegistryError
        | FileErr of FileError
        | SerializationErr of SerializationError
        | WcfErr of WcfError
        | DbErr of DbError
        | ProcessStartedErr of ProcessStartedError
        | MessagingServiceErr of MessagingServiceError
        | MessagingClientErr of MessagingClientError
        | WorkerNodeErr of WorkerNodeError
        | WorkerNodeServiceErr of WorkerNodeServiceError

        static member (+) (a, b) =
            match a, b with
            | AggregateErr (x, w), AggregateErr (y, z) -> AggregateErr (x, w @ (y :: z))
            | AggregateErr (x, w), _ -> AggregateErr (x, w @ [b])
            | _, AggregateErr (y, z) -> AggregateErr (a, y :: z)
            | _ -> AggregateErr (a, [b])

        member a.add b = a + b


    let (>->) s1 s2 =
        match s1() with
        | Ok() -> s2
        | Error e -> fun () -> Error e


    let evaluate f = f()


    /// Encapsulation of logging information
    type ClmInfo =
        | ClmInfo of string

        static member create a = sprintf "%A" a |> ClmInfo


    type UnitResult = Result<unit, ClmError>
    type ClmResult<'T> = Result<'T, ClmError>
    type ListResult<'T> = Result<list<Result<'T, ClmError>>, ClmError>


    /// ! Note that we cannot elevate to Result here as it will broaden the scope !
    /// Folds list<ClmError> in a single ClmError.
    let foldErrors (a : list<ClmError>) =
        match a with
        | [] -> None
        | h :: t -> t |> List.fold (fun acc r -> r + acc) h |> Some


    /// Converts and error option into a unit result.
    let toUnitResult fo =
        match fo with
        | None -> Ok()
        | Some f -> Error f


    /// Folds list<ClmError>, then converts to UnitResult.
    let foldToUnitResult = foldErrors >> toUnitResult


    let addError v (f : ClmError) =
        match v with
        | Ok r -> Ok r
        | Error e -> Error (f + e)


    let combineUnitResults (r1 : UnitResult) (r2 : UnitResult) =
        match r1, r2 with
        | Ok(), Ok() -> Ok()
        | Error e1, Ok() -> Error e1
        | Ok(), Error e2 -> Error e2
        | Error e1, Error e2 -> Error (e1 + e2)


    let foldUnitResults (r : list<UnitResult>) =
        let rec fold acc w =
            match w with
            | [] -> acc
            | h :: t -> fold (combineUnitResults h acc) t

        fold (Ok()) r


    type ClmErrorInfo =
        {
            errorId : ErrorId
            traceInfo : TraceInfo
            error : ClmError
        }
