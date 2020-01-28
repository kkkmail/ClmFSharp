namespace ClmSys

open System
open VersionInfo
open GeneralData
open MessagingData
open Fake.Windows
open ClmSys
open Rop
open Logging
open PartitionerData
open GeneralErrors
open GeneralPrimitives

module Registry =

    let private serviceAddressKey = "ServiceAddress"
    let private servicePortKey = "ServicePort"
    let private messagingClientIdKey = "ClientId"
    let private partitionerMessagingClientIdKey = "PartitionerId"
    let private usePartitionerKey = "UsePartitioner"
    let private numberOfCoresKey = "NumberOfCores"
    let private wrkInactiveKey = "IsInactive"
    let private contGenServiceAddressKey = "ContGenServiceAddress"
    let private contGenServicePortKey = "ContGenServicePort"
    let private contGenMinUsefulEeKey = "ContGenMinUsefulEe"

    let contGenServiceName ="ContGenService" |> MessagingClientName
    let workerNodeServiceName ="WorkerNodeService" |> MessagingClientName
    let partitionerServiceName ="PartitionerService" |> MessagingClientName
    let solverRunnerName ="SolverRunner" |> MessagingClientName
    let messagingServiceName ="MessagingService" |> MessagingClientName
    let messagingAdmName ="MessagingAdm" |> MessagingClientName

    let private formatSubKey subKey = (sprintf "subKey: '%s'" subKey)
    let private formatSubKeyValue subKey value = (sprintf "subKey: '%s', value = '%s'." subKey value)
    let private formatSubKeyKey subKey key = (sprintf "subKey: '%s', key = '%s'." subKey key)

    let private getTopSubKey (VersionNumber v) = "SOFTWARE" + "\\" + SystemName + "\\" + v
    let private getMessagingClientSubKey v (MessagingClientName c) = (getTopSubKey v) + "\\" + c

    let addError f e = ((f |> RegistryErr) + e) |> Error
    let private toError e = e |> RegistryErr |> Error
    let private toErrorInfo f (v : VersionNumber) (c : MessagingClientName) s = { version = v.value; client = c.value; data = s } |> f
    let private toErrorInfoNoData f (v : VersionNumber) (c : MessagingClientName) = { version = v.value; client = c.value; data = EmptyString } |> f


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


    // Messaging Client
    let tryGetMessagingClientAddress v c =
        tryGetRegistryValue (getMessagingClientSubKey v c) serviceAddressKey |> Rop.bindSuccess ServiceAddress


    let trySetMessagingClientAddress v c (ServiceAddress a) =
        match tryCreateRegistrySubKey (getMessagingClientSubKey v c) with
        | Ok() -> trySetRegistryValue (getMessagingClientSubKey v c) serviceAddressKey a
        | Error e -> Error e


    let tryGetMessagingClientPort v c =
        match tryGetRegistryValue (getMessagingClientSubKey v c) servicePortKey with
        | Ok s ->
            match Int32.TryParse s with
            | true, v -> ServicePort v |> Ok
            | false, _ -> toErrorInfo GetMessagingClientPortError v c s |> toError
        | Error e -> (toErrorInfoNoData GetMessagingClientPortError v c, e) ||> addError


    let trySetMessagingClientPort v c (ServicePort p) =
        match tryCreateRegistrySubKey (getMessagingClientSubKey v c) with
        | Ok() -> trySetRegistryValue (getMessagingClientSubKey v c) servicePortKey (p.ToString())
        | Error e -> Error e


    let tryGetMessagingClientId v c =
        match tryGetRegistryValue (getMessagingClientSubKey v c) messagingClientIdKey with
        | Ok s ->
            match Guid.TryParse s with
            | true, v -> MessagingClientId v |> Ok
            | false, _ -> toErrorInfo GetMessagingClientIdError v c s |> toError
        | Error e -> (toErrorInfoNoData GetMessagingClientIdError v c, e) ||> addError


    let trySetMessagingClientId v c (MessagingClientId i) =
        match tryCreateRegistrySubKey (getMessagingClientSubKey v c) with
        | Ok() -> trySetRegistryValue (getMessagingClientSubKey v c) messagingClientIdKey (i.ToString())
        | Error e -> Error e


    /// Partitioner - Messaging Client Id
    let tryGetPartitionerMessagingClientId v c =
        match tryGetRegistryValue (getMessagingClientSubKey v c) partitionerMessagingClientIdKey with
        | Ok s ->
            match Guid.TryParse s with
            | true, v -> v |> MessagingClientId |> PartitionerId |> Ok
            | false, _ -> toErrorInfo GetPartitionerMessagingClientIdError v c s |> toError
        | Error e -> Error e


    let trySetPartitionerMessagingClientId v c (PartitionerId (MessagingClientId i)) =
        match tryCreateRegistrySubKey (getMessagingClientSubKey v c) with
        | Ok() -> trySetRegistryValue (getMessagingClientSubKey v c) partitionerMessagingClientIdKey (i.ToString())
        | Error e -> Error e


    let tryGetUsePartitioner v c =
        match tryGetRegistryValue (getMessagingClientSubKey v c) usePartitionerKey with
        | Ok s ->
            match Boolean.TryParse s with
            | true, v -> Ok v
            | false, _ -> toErrorInfo GetUsePartitionerError v c s |> toError
        |  Error e -> Error e


    let trySetUsePartitioner v c (u : bool) =
        match tryCreateRegistrySubKey (getMessagingClientSubKey v c) with
        | Ok() -> trySetRegistryValue (getMessagingClientSubKey v c) usePartitionerKey (u.ToString())
        | Error e -> Error e


    let tryGetNumberOfCores v c =
        match tryGetRegistryValue (getMessagingClientSubKey v c) numberOfCoresKey with
        | Ok s ->
            match Int32.TryParse s with
            | true, v -> Ok v
            | false, _ -> toErrorInfo GetNumberOfCoresError v c s |> toError
        | Error e -> Error e


    let trySetNumberOfCores v c n =
        match tryCreateRegistrySubKey (getMessagingClientSubKey v c) with
        | Ok() -> trySetRegistryValue (getMessagingClientSubKey v c) numberOfCoresKey (n.ToString())
        | Error e -> Error e


    let tryGetWrkInactive v c =
        match tryGetRegistryValue (getMessagingClientSubKey v c) wrkInactiveKey with
        | Ok s ->
            match Boolean.TryParse s with
            | true, v -> Ok v
            | false, _ -> toErrorInfo GetWrkInactiveError v c s |> toError
        | Error e -> Error e


    let trySetWrkInactive v c n =
        match tryCreateRegistrySubKey (getMessagingClientSubKey v c) with
        | Ok() -> trySetRegistryValue (getMessagingClientSubKey v c) wrkInactiveKey (n.ToString())
        | Error e -> Error e


    let tryGetContGenServiceAddress v c =
        tryGetRegistryValue (getMessagingClientSubKey v c) contGenServiceAddressKey |> Rop.bindSuccess ServiceAddress


    let trySetContGenServiceAddress v c (ServiceAddress a) =
        match tryCreateRegistrySubKey (getMessagingClientSubKey v c) with
        | Ok() -> trySetRegistryValue (getMessagingClientSubKey v c) contGenServiceAddressKey a
        | Error e -> Error e


    let tryGetContGenServicePort v c =
        match tryGetRegistryValue (getMessagingClientSubKey v c) contGenServicePortKey with
        | Ok s ->
            match Int32.TryParse s with
            | true, v -> ServicePort v |> Ok
            | false, _ -> toErrorInfo GetContGenServicePortError v c s |> toError
        | Error e -> Error e


    let trySetContGenServicePort v c (ServicePort p) =
        match tryCreateRegistrySubKey (getMessagingClientSubKey v c) with
        | Ok() -> trySetRegistryValue (getMessagingClientSubKey v c) contGenServicePortKey (p.ToString())
        | Error e -> Error e


    let tryGetContGenMinUsefulEe v c =
        match tryGetRegistryValue (getMessagingClientSubKey v c) contGenMinUsefulEeKey with
        | Ok s ->
            match Double.TryParse s with
            | true, v -> v |> MinUsefulEe |> Ok
            | false, _ -> toErrorInfo GetContGenMinUsefulEeError v c s |> toError
        | Error e -> Error e


    let trySetContGenMinUsefulEe v c (MinUsefulEe p) =
        match tryCreateRegistrySubKey (getMessagingClientSubKey v c) with
        | Ok() -> trySetRegistryValue (getMessagingClientSubKey v c) contGenMinUsefulEeKey (p.ToString())
        | Error e -> Error e


    // Getters OR defaults
    let getMsgServerAddressImpl getter logger version name p =
        match getter p with
        | Some a -> a
        | None ->
            match tryGetMessagingClientAddress version name with
            | Ok a -> a
            | Error _ -> ServiceAddress.defaultMessagingServerValue


    let getMsgServerPortImpl getter logger version name p =
        match getter p with
        | Some a -> a
        | None ->
            match tryGetMessagingClientPort version name with
            | Ok a -> a
            | Error _ -> ServicePort.defaultMessagingServerValue


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
