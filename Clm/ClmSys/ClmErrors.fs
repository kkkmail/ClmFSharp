namespace ClmSys

open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open VersionInfo
open GeneralErrors
open ContGenErrors
open AsynRunErrors
open MessagingErrors
open WorkerNodeErrors
open PartitionerErrors


module ClmErrors =

    /// All errors known in the system.
    type ClmError =
        | AggregateErr of ClmError * List<ClmError>
        //| UnhandledExn of exn
        //| UnknownErr of string
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
        | PartitionerErr of PartitionerError

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
    type StateWithResult<'T> = 'T * UnitResult


    /// ! Note that we cannot elevate to Result here as it will broaden the scope !
    /// Folds list<ClmError> in a single ClmError.
    let foldErrors (a : list<ClmError>) =
        match a with
        | [] -> None
        | h :: t -> t |> List.fold (fun acc r -> r + acc) h |> Some


    /// Converts an error option into a unit result.
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


    let toErrorOption f g (r : UnitResult) =
        match r with
        | Ok() -> None
        | Error e -> Some ((f g) + e)


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
