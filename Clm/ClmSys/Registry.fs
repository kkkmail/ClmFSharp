namespace ClmSys

open System
open VersionInfo
open GeneralData
open Fake.Windows
open ClmSys
open PartitionerData
open GeneralErrors
open GeneralPrimitives
open MessagingPrimitives
open PartitionerPrimitives
open ContGenPrimitives
open ClmErrors
open WorkerNodePrimitives

module Registry =

    type RegistryKeyName =
        | RegistryKeyName of string

        member this.value = let (RegistryKeyName v) = this in v


    let private msgServiceAddressKey = "MsgServiceAddress"
    let private msgServicePortKey = "MsgServicePort"
    let private messagingClientIdKey = "ClientId"
    let private partitionerMessagingClientIdKey = "PartitionerId"
    let private usePartitionerKey = "UsePartitioner"
    let private workerNodeNameKey = "WorkerNodeName"
    let private numberOfCoresKey = "NumberOfCores"
    let private wrkInactiveKey = "IsInactive"
    let private contGenServiceAddressKey = "ContGenServiceAddress"
    let private contGenServicePortKey = "ContGenServicePort"
    let private contGenMinUsefulEeKey = "ContGenMinUsefulEe"
    let private wrkNodeServiceAddressKey = "WorkerNodeServiceAddress"
    let private wrkNodeServicePortKey = "WorkerNodeServicePort"

    let contGenServiceRegistryName ="ContGenService" |> RegistryKeyName
    let workerNodeServiceRegistryName ="WorkerNodeService" |> RegistryKeyName
    let partitionerServiceRegistryName ="PartitionerService" |> RegistryKeyName
    let solverRunnerRegistryName ="SolverRunner" |> RegistryKeyName
    let messagingServiceRegistryName ="MessagingService" |> RegistryKeyName
    let messagingAdmRegistryName ="MessagingAdm" |> RegistryKeyName

    let private formatSubKey subKey = (sprintf "subKey: '%s'" subKey)
    let private formatSubKeyValue subKey value = (sprintf "subKey: '%s', value = '%s'." subKey value)
    let private formatSubKeyKey subKey key = (sprintf "subKey: '%s', key = '%s'." subKey key)

    let private getTopSubKey (VersionNumber v) = "SOFTWARE" + "\\" + SystemName + "\\" + v
    let private getServiceSubKey v (RegistryKeyName c) = (getTopSubKey v) + "\\" + c

    let addError f e = ((f |> RegistryErr) + e) |> Error
    let private toError e = e |> RegistryErr |> Error
    let private toErrorInfo f (v : VersionNumber) (c : RegistryKeyName) s = { version = v.value; client = c.value; data = s } |> f
    let private toErrorInfoNoData f (v : VersionNumber) (c : RegistryKeyName) = { version = v.value; client = c.value; data = EmptyString } |> f


    let private tryCreateRegistrySubKey subKey =
        try
            Registry.createRegistrySubKey Registry.HKEYLocalMachine subKey
            Ok()
        with
        | e -> ((formatSubKey subKey), e) |> CreateRegistrySubKeyError |> toError


    let private trySetRegistryValue subKey key value =
        try
            Registry.setRegistryValue Registry.HKEYLocalMachine subKey key value
            Ok()
        with
        | e -> ((formatSubKeyValue subKey value), e) |> SetRegistryValueError |> toError


    //let private tryGetRegistryValueNames (logger : Logger) subKey =
    //    try
    //        Registry.getRegistryValueNames Registry.HKEYLocalMachine subKey |> Some
    //    with
    //        | e ->
    //            logger.logExn (formatSubKey subKey) e
    //            None


    let private tryGetRegistryValue subKey key =
        try
             Registry.getRegistryValue Registry.HKEYLocalMachine subKey key |> Ok
        with
        | e -> ((formatSubKeyKey subKey key), e) |> GetRegistryValueError |> toError


    //let private valueExistsForKey (logger : Logger) subKey key =
    //    try
    //        Registry.valueExistsForKey Registry.HKEYLocalMachine subKey key
    //    with
    //        | e ->
    //            logger.logExn (formatSubKeyKey subKey key) e
    //            false


    //let private tryDeleteRegistryValue (logger : Logger) subKey key =
    //    try
    //         Registry.deleteRegistryValue Registry.HKEYLocalMachine subKey key |> Some
    //    with
    //        | e ->
    //            logger.logExn (formatSubKeyKey subKey key) e
    //            None


    //let private tryDeleteRegistrySubKey (logger : Logger) subKey =
    //    try
    //         Registry.deleteRegistrySubKey Registry.HKEYLocalMachine subKey |> Some
    //    with
    //        | e ->
    //            logger.logExn (formatSubKey subKey) e
    //            None


    let tryGetMessagingServiceAddress v c =
        tryGetRegistryValue (getServiceSubKey v c) msgServiceAddressKey
        |> Rop.bindSuccess ServiceAddress
        |> Rop.bindSuccess MessagingServiceAddress


    let trySetMessagingServiceAddress v c (MessagingServiceAddress (ServiceAddress a)) =
        match tryCreateRegistrySubKey (getServiceSubKey v c) with
        | Ok() -> trySetRegistryValue (getServiceSubKey v c) msgServiceAddressKey a
        | Error e -> Error e


    let tryGetMessagingServicePort v c =
        match tryGetRegistryValue (getServiceSubKey v c) msgServicePortKey with
        | Ok s ->
            match Int32.TryParse s with
            | true, v -> v |> ServicePort |> MessagingServicePort |> Ok
            | false, _ -> toErrorInfo GetMessagingClientPortError v c s |> toError
        | Error e -> (toErrorInfoNoData GetMessagingClientPortError v c, e) ||> addError


    let trySetMessagingServicePort v c (MessagingServicePort (ServicePort p)) =
        match tryCreateRegistrySubKey (getServiceSubKey v c) with
        | Ok() -> trySetRegistryValue (getServiceSubKey v c) msgServicePortKey (p.ToString())
        | Error e -> Error e


    let tryGetMessagingClientId v c =
        match tryGetRegistryValue (getServiceSubKey v c) messagingClientIdKey with
        | Ok s ->
            match Guid.TryParse s with
            | true, v -> MessagingClientId v |> Ok
            | false, _ -> toErrorInfo GetMessagingClientIdError v c s |> toError
        | Error e -> (toErrorInfoNoData GetMessagingClientIdError v c, e) ||> addError


    let trySetMessagingClientId v c (MessagingClientId i) =
        match tryCreateRegistrySubKey (getServiceSubKey v c) with
        | Ok() -> trySetRegistryValue (getServiceSubKey v c) messagingClientIdKey (i.ToString())
        | Error e -> Error e


    /// Partitioner - Messaging Client Id
    let tryGetPartitionerMessagingClientId v c =
        match tryGetRegistryValue (getServiceSubKey v c) partitionerMessagingClientIdKey with
        | Ok s ->
            match Guid.TryParse s with
            | true, v -> v |> MessagingClientId |> PartitionerId |> Ok
            | false, _ -> toErrorInfo GetPartitionerMessagingClientIdError v c s |> toError
        | Error e -> Error e


    let trySetPartitionerMessagingClientId v c (PartitionerId (MessagingClientId i)) =
        match tryCreateRegistrySubKey (getServiceSubKey v c) with
        | Ok() -> trySetRegistryValue (getServiceSubKey v c) partitionerMessagingClientIdKey (i.ToString())
        | Error e -> Error e


    let tryGetUsePartitioner v c =
        match tryGetRegistryValue (getServiceSubKey v c) usePartitionerKey with
        | Ok s ->
            match Boolean.TryParse s with
            | true, v -> Ok v
            | false, _ -> toErrorInfo GetUsePartitionerError v c s |> toError
        |  Error e -> Error e


    let trySetUsePartitioner v c (u : bool) =
        match tryCreateRegistrySubKey (getServiceSubKey v c) with
        | Ok() -> trySetRegistryValue (getServiceSubKey v c) usePartitionerKey (u.ToString())
        | Error e -> Error e


    let tryGetWorkerNodeName v c =
        tryGetRegistryValue (getServiceSubKey v c) workerNodeNameKey


    let trySetWorkerNodeName v c (WorkerNodeName n) =
        match tryCreateRegistrySubKey (getServiceSubKey v c) with
        | Ok() -> trySetRegistryValue (getServiceSubKey v c) workerNodeNameKey n
        | Error e -> Error e


    let tryGetWorkerNodeServiceAddress v c =
        tryGetRegistryValue (getServiceSubKey v c) wrkNodeServiceAddressKey
        |> Rop.bindSuccess ServiceAddress
        |> Rop.bindSuccess WorkerNodeServiceAddress


    let trySetWorkerNodeServiceAddress v c (WorkerNodeServiceAddress (ServiceAddress a)) =
        match tryCreateRegistrySubKey (getServiceSubKey v c) with
        | Ok() -> trySetRegistryValue (getServiceSubKey v c) wrkNodeServiceAddressKey a
        | Error e -> Error e


    let tryGetWorkerNodeServicePort v c =
        match tryGetRegistryValue (getServiceSubKey v c) wrkNodeServicePortKey with
        | Ok s ->
            match Int32.TryParse s with
            | true, v -> v |> ServicePort |> WorkerNodeServicePort |> Ok
            | false, _ -> toErrorInfo GetMorkerNodeClientPortError v c s |> toError
        | Error e -> (toErrorInfoNoData GetMorkerNodeClientPortError v c, e) ||> addError


    let trySetWorkerNodeServicePort v c (WorkerNodeServicePort (ServicePort p)) =
        match tryCreateRegistrySubKey (getServiceSubKey v c) with
        | Ok() -> trySetRegistryValue (getServiceSubKey v c) wrkNodeServicePortKey (p.ToString())
        | Error e -> Error e


    let tryGetNumberOfCores v c =
        match tryGetRegistryValue (getServiceSubKey v c) numberOfCoresKey with
        | Ok s ->
            match Int32.TryParse s with
            | true, v -> Ok v
            | false, _ -> toErrorInfo GetNumberOfCoresError v c s |> toError
        | Error e -> Error e


    let trySetNumberOfCores v c n =
        match tryCreateRegistrySubKey (getServiceSubKey v c) with
        | Ok() -> trySetRegistryValue (getServiceSubKey v c) numberOfCoresKey (n.ToString())
        | Error e -> Error e


    let tryGetWrkInactive v c =
        match tryGetRegistryValue (getServiceSubKey v c) wrkInactiveKey with
        | Ok s ->
            match Boolean.TryParse s with
            | true, v -> Ok v
            | false, _ -> toErrorInfo GetWrkInactiveError v c s |> toError
        | Error e -> Error e


    let trySetWrkInactive v c n =
        match tryCreateRegistrySubKey (getServiceSubKey v c) with
        | Ok() -> trySetRegistryValue (getServiceSubKey v c) wrkInactiveKey (n.ToString())
        | Error e -> Error e


    let tryGetContGenServiceAddress v c =
        tryGetRegistryValue (getServiceSubKey v c) contGenServiceAddressKey
        |> Rop.bindSuccess ServiceAddress
        |> Rop.bindSuccess ContGenServiceAddress


    let trySetContGenServiceAddress v c (ContGenServiceAddress (ServiceAddress a)) =
        match tryCreateRegistrySubKey (getServiceSubKey v c) with
        | Ok() -> trySetRegistryValue (getServiceSubKey v c) contGenServiceAddressKey a
        | Error e -> Error e


    let tryGetContGenServicePort v c =
        match tryGetRegistryValue (getServiceSubKey v c) contGenServicePortKey with
        | Ok s ->
            match Int32.TryParse s with
            | true, v -> v |> ServicePort |> ContGenServicePort |> Ok
            | false, _ -> toErrorInfo GetContGenServicePortError v c s |> toError
        | Error e -> Error e


    let trySetContGenServicePort v c (ContGenServicePort (ServicePort p)) =
        match tryCreateRegistrySubKey (getServiceSubKey v c) with
        | Ok() -> trySetRegistryValue (getServiceSubKey v c) contGenServicePortKey (p.ToString())
        | Error e -> Error e


    let tryGetContGenMinUsefulEe v c =
        match tryGetRegistryValue (getServiceSubKey v c) contGenMinUsefulEeKey with
        | Ok s ->
            match Double.TryParse s with
            | true, v -> v |> MinUsefulEe |> Ok
            | false, _ -> toErrorInfo GetContGenMinUsefulEeError v c s |> toError
        | Error e -> Error e


    let trySetContGenMinUsefulEe v c (MinUsefulEe p) =
        match tryCreateRegistrySubKey (getServiceSubKey v c) with
        | Ok() -> trySetRegistryValue (getServiceSubKey v c) contGenMinUsefulEeKey (p.ToString())
        | Error e -> Error e


    // Getters OR defaults
    let getMsgServiceAddressImpl getter logger version name p =
        match getter p with
        | Some a -> a
        | None ->
            match tryGetMessagingServiceAddress version name with
            | Ok a -> a
            | Error _ -> MessagingServiceAddress.defaultValue


    let getMsgServicePortImpl getter logger version name p =
        match getter p with
        | Some a -> a
        | None ->
            match tryGetMessagingServicePort version name with
            | Ok a -> a
            | Error _ -> MessagingServicePort.defaultValue


    let getContGenServiceAddressImpl getter logger version name p =
        match getter p with
        | Some a -> a
        | None ->
            match tryGetContGenServiceAddress version name with
            | Ok a -> a
            | Error _ -> ContGenServiceAddress.defaultValue


    let getContGenServicePortImpl getter logger version name p =
        match getter p with
        | Some a -> a
        | None ->
            match tryGetContGenServicePort version name with
            | Ok a -> a
            | Error _ -> ContGenServicePort.defaultValue


    let getWorkerNodeServiceAddressImpl getter logger version name p =
        match getter p with
        | Some a -> a
        | None ->
            match tryGetWorkerNodeServiceAddress version name with
            | Ok a -> a
            | Error _ -> WorkerNodeServiceAddress.defaultValue


    let getWorkerNodeServicePortImpl getter logger version name p =
        match getter p with
        | Some a -> a
        | None ->
            match tryGetWorkerNodeServicePort version name with
            | Ok a -> a
            | Error _ -> WorkerNodeServicePort.defaultValue


    let getPartitionerImpl getter logger version name p =
        match getter p with
        | Some x -> x
        | None ->
            match tryGetPartitionerMessagingClientId version name with
            | Ok x -> x
            | Error _ -> defaultPartitionerId


    let getUsePartitionerImpl getter logger version name p =
        match getter p with
        | Some x -> x
        | None ->
            match tryGetUsePartitioner version name with
            | Ok x -> x
            | Error _ -> false
