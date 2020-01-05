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


    type ClmError =
        | UnhandledExn of exn
        | UnknownErr of string
        | FileErr of FileError
        | SerializationErr of SerializationError
        | WcfErr of WcfError
        | DbErr of DbError
        | ProcessStartedErr of ProcessStartedError


    type ClmErrorInfo =
        {
            errorId : ErrorId
            traceInfo : TraceInfo
            error : ClmError
        }
