namespace ClmSys

open System
open VersionInfo
open GeneralData
open MessagingData
open Fake.Windows
open ClmSys
open ClmSys.Rop
open ClmSys.Logging
open ClmSys.PartitionerData
open ClmSys.GeneralErrors

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

    let private toError e = e |> RegistryErr |> Error


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
            | false, _ -> sprintf "Unable to parse '%s' for version number: %A, client name: %A" s v c |> GetMessagingClientPortError |> toError
        | Error e -> Error e


    let trySetMessagingClientPort (logger : Logger) v c (ServicePort p) =
        match tryCreateRegistrySubKey logger (getMessagingClientSubKey v c) with
        | Some _ -> trySetRegistryValue logger (getMessagingClientSubKey v c) servicePortKey (p.ToString())
        | None -> None


    let tryGetMessagingClientId (logger : Logger) v c =
        match tryGetRegistryValue logger (getMessagingClientSubKey v c) messagingClientIdKey with
        | Some s ->
            match Guid.TryParse s with
            | true, v -> MessagingClientId v |> Some
            | false, _ -> None
        | None -> None


    let trySetMessagingClientId (logger : Logger) v c (MessagingClientId i) =
        match tryCreateRegistrySubKey logger (getMessagingClientSubKey v c) with
        | Some _ -> trySetRegistryValue logger (getMessagingClientSubKey v c) messagingClientIdKey (i.ToString())
        | None -> None


    /// Partitioner - Messaging Client Id
    let tryGetPartitionerMessagingClientId (logger : Logger) v c =
        match tryGetRegistryValue logger (getMessagingClientSubKey v c) partitionerMessagingClientIdKey with
        | Some s ->
            match Guid.TryParse s with
            | true, v -> v |> MessagingClientId |> PartitionerId |> Some
            | false, _ -> None
        | None -> None


    let trySetPartitionerMessagingClientId (logger : Logger) v c (PartitionerId (MessagingClientId i)) =
        match tryCreateRegistrySubKey logger (getMessagingClientSubKey v c) with
        | Some _ -> trySetRegistryValue logger (getMessagingClientSubKey v c) partitionerMessagingClientIdKey (i.ToString())
        | None -> None


    let tryGetUsePartitioner (logger : Logger) v c =
        match tryGetRegistryValue logger (getMessagingClientSubKey v c) usePartitionerKey with
        | Some s ->
            match Boolean.TryParse s with
            | true, v -> Some v
            | false, _ -> None
        | None -> None


    let trySetUsePartitioner (logger : Logger) v c (u : bool) =
        match tryCreateRegistrySubKey logger (getMessagingClientSubKey v c) with
        | Some _ -> trySetRegistryValue logger (getMessagingClientSubKey v c) usePartitionerKey (u.ToString())
        | None -> None


    let tryGetNumberOfCores (logger : Logger) v c =
        match tryGetRegistryValue logger (getMessagingClientSubKey v c) numberOfCoresKey with
        | Some s ->
            match Int32.TryParse s with
            | true, v -> Some v
            | false, _ -> None
        | None -> None


    let trySetNumberOfCores (logger : Logger) v c n =
        match tryCreateRegistrySubKey logger (getMessagingClientSubKey v c) with
        | Some _ -> trySetRegistryValue logger (getMessagingClientSubKey v c) numberOfCoresKey (n.ToString())
        | None -> None


    let tryGetWrkInactive (logger : Logger) v c =
        match tryGetRegistryValue logger (getMessagingClientSubKey v c) wrkInactiveKey with
        | Some s ->
            match Boolean.TryParse s with
            | true, v -> Some v
            | false, _ -> None
        | None -> None


    let trySetWrkInactive (logger : Logger) v c n =
        match tryCreateRegistrySubKey logger (getMessagingClientSubKey v c) with
        | Some _ -> trySetRegistryValue logger (getMessagingClientSubKey v c) wrkInactiveKey (n.ToString())
        | None -> None


    let tryGetContGenServiceAddress (logger : Logger) v c =
        match tryGetRegistryValue logger (getMessagingClientSubKey v c) contGenServiceAddressKey with
        | Some s -> ServiceAddress s |> Some
        | None -> None


    let trySetContGenServiceAddress (logger : Logger) v c (ServiceAddress a) =
        match tryCreateRegistrySubKey logger (getMessagingClientSubKey v c) with
        | Some _ -> trySetRegistryValue logger (getMessagingClientSubKey v c) contGenServiceAddressKey a
        | None -> None


    let tryGetContGenServicePort (logger : Logger) v c =
        match tryGetRegistryValue logger (getMessagingClientSubKey v c) contGenServicePortKey with
        | Some s ->
            match Int32.TryParse s with
            | true, v -> ServicePort v |> Some
            | false, _ -> None
        | None -> None


    let trySetContGenServicePort (logger : Logger) v c (ServicePort p) =
        match tryCreateRegistrySubKey logger (getMessagingClientSubKey v c) with
        | Some _ -> trySetRegistryValue logger (getMessagingClientSubKey v c) contGenServicePortKey (p.ToString())
        | None -> None


    let tryGetContGenMinUsefulEe (logger : Logger) v c =
        match tryGetRegistryValue logger (getMessagingClientSubKey v c) contGenMinUsefulEeKey with
        | Some s ->
            match Double.TryParse s with
            | true, v -> v |> MinUsefulEe |> Some
            | false, _ -> None
        | None -> None


    let trySetContGenMinUsefulEe (logger : Logger) v c (MinUsefulEe p) =
        match tryCreateRegistrySubKey logger (getMessagingClientSubKey v c) with
        | Some _ -> trySetRegistryValue logger (getMessagingClientSubKey v c) contGenMinUsefulEeKey (p.ToString())
        | None -> None


    // Getters OR defaults
    let getMsgServerAddressImpl getter logger version name p =
        match getter p with
        | Some a -> a
        | None ->
            match tryGetMessagingClientAddress logger version name with
            | Some a -> a
            | None -> ServiceAddress.defaultMessagingServerValue


    let getMsgServerPortImpl getter logger version name p =
        match getter p with
        | Some a -> a
        | None ->
            match tryGetMessagingClientPort logger version name with
            | Some a -> a
            | None -> ServicePort.defaultMessagingServerValue


    let getPartitionerImpl getter logger version name p =
        match getter p with
        | Some x -> x
        | None ->
            match tryGetPartitionerMessagingClientId logger version name with
            | Some x -> x
            | None -> defaultPartitionerId


    let getUsePartitionerImpl getter logger version name p =
        match getter p with
        | Some x -> x
        | None ->
            match tryGetUsePartitioner logger version name with
            | Some x -> x
            | None -> false
