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
        | GetFolderNameException of exn
        | GetFileNameException of exn
        | FileNotFound of string
        | ReadFileException of exn
        | WriteFileException of exn
        | DeleteFileException of exn
        | GetObjectIdsException of exn


    type JsonParseError =
        | InvalidStructure of string


    //type ReadJsonError =
    //    | ReadFileError of ReadFileError
    //    | JsonParseError of JsonParseError


    type SerializationError =
        | SerializationException of exn
        | DeserializationException of exn


    type WcfError =
        | WcfException of exn
        | WcfSerializationError of SerializationError


    type Err =
        | ExceptionErr of exn
        | UnknownErr of string
        //| ReadFileErr of ReadFileError
        //| JsonParseErr of JsonParseError
        //| ReadJsonErr of ReadJsonError
        | SerializationErr of SerializationError
        | WcfErr of WcfError


    type ClmError =
        {
            errorId : ErrorId
            traceInfo : TraceInfo
            err : Err
        }
