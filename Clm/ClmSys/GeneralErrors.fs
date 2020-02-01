namespace ClmSys

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

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
        | GeneralFileExn of exn
        | GetFolderNameExn of exn
        | GetFileNameExn of exn
        | FileNotFoundErr of string
        | ReadFileExn of exn
        | WriteFileExn of exn
        | DeleteFileExn of exn
        | GetObjectIdsExn of exn
        | CreateChartsExn of exn
        | SaveChartsExn of exn


    type JsonParseError =
        | InvalidStructureErr of string


    type SerializationError =
        | SerializationExn of exn
        | DeserializationExn of exn


    type WcfError =
        | WcfExn of exn
        | WcfSerializationErr of SerializationError


    type DbError =
        | DbExn of exn
        | LoadModelDataError of Guid
        | SaveResultDataErr of Guid
        | LoadResultDataErr of Guid
        | LoadClmDefaultValueErr of Int64
        | UpsertClmDefaultValueErr of Int64
        | LoadClmTaskByDefaultErr of Int64
        | LoadClmTaskErr of Guid
        | UpdateClmTaskErr of Guid
        | UpdateModelDataErr of Guid
        | ClmTaskTryCreatErr of Guid
        | ModelDataTryCreateErr of Guid
        | DeleteRunQueueEntryErr of Guid
        | MapRunQueueErr of Guid


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


    type ClmEventHandlerError =
        | UnhandledException of Guid * exn
        | StillRunningError of Guid
