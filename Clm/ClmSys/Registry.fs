namespace ClmSys

open System
open VersionInfo
open GeneralData
open MessagingData
open Fake.Windows
open ClmSys.Logging
open ClmSys.PartitionerData

module Registry =

    let private serviceAddressKey = "ServiceAddress"
    let private servicePortKey = "ServicePort"
    let private messagingClientIdKey = "ClientId"
    let private partitionerMessagingClientIdKey = "PartitionerId"
    let private numberOfCoresKey = "NumberOfCores"
    let private contGenServiceAddressKey = "ContGenServiceAddress"
    let private contGenServicePortKey = "ContGenServicePort"
    let private storageMessagingClientIdKey = "StorageId"

    let contGenServiceName ="ContGenService" |> MessagingClientName
    let workerNodeServiceName ="WorkerNodeService" |> MessagingClientName
    let partitionerServiceName ="PartitionerService" |> MessagingClientName
    let solverRunnerName ="SolverRunner" |> MessagingClientName

    let private formatSubKey subKey = (sprintf "subKey: '%s'" subKey)
    let private formatSubKeyValue subKey value = (sprintf "subKey: '%s', value = '%s'." subKey value)
    let private formatSubKeyKey subKey key = (sprintf "subKey: '%s', key = '%s'." subKey key)

    let private getTopSubKey (VersionNumber v) = "SOFTWARE" + "\\" + SystemName + "\\" + v
    let private getMessagingServerSubKey v = (getTopSubKey v) + "\\" + "Messaging" + "\\" + "Server"
    let private getMessagingClientSubKey v (MessagingClientName c) = (getTopSubKey v) + "\\" + "Messaging" + "\\" + "Client" + "\\" + c


    let private tryCreateRegistrySubKey (logger : Logger) subKey =
        try
            Registry.createRegistrySubKey Registry.HKEYLocalMachine subKey
            Some ()
        with
            | e ->
                logger.logExn (formatSubKey subKey) e
                None


    let private trySetRegistryValue (logger : Logger) subKey key value =
        try
            Registry.setRegistryValue Registry.HKEYLocalMachine subKey key value
            Some ()
        with
            | e ->
                logger.logExn (formatSubKeyValue subKey value) e
                None


    let private tryGetRegistryValueNames (logger : Logger) subKey =
        try
            Registry.getRegistryValueNames Registry.HKEYLocalMachine subKey |> Some
        with
            | e ->
                logger.logExn (formatSubKey subKey) e
                None


    let private tryGetRegistryValue (logger : Logger) subKey key =
        try
             Registry.getRegistryValue Registry.HKEYLocalMachine subKey key |> Some
        with
            | e ->
                logger.logExn (formatSubKeyKey subKey key) e
                None


    let private valueExistsForKey (logger : Logger) subKey key =
        try
            Registry.valueExistsForKey Registry.HKEYLocalMachine subKey key
        with
            | e ->
                logger.logExn (formatSubKeyKey subKey key) e
                false


    let private tryDeleteRegistryValue (logger : Logger) subKey key =
        try
             Registry.deleteRegistryValue Registry.HKEYLocalMachine subKey key |> Some
        with
            | e ->
                logger.logExn (formatSubKeyKey subKey key) e
                None


    let private tryDeleteRegistrySubKey (logger : Logger) subKey =
        try
             Registry.deleteRegistrySubKey Registry.HKEYLocalMachine subKey |> Some
        with
            | e ->
                logger.logExn (formatSubKey subKey) e
                None


    //let tryGetNodeId logger v =
    //    match tryGetRegistryValue logger (getNodeSubKey v) nodeIdKey with
    //    | Some s ->
    //        match Guid.TryParse s with
    //        | true, v -> NodeId v |> Some
    //        | false, _ -> None
    //    | None -> None


    //let trySetNodeId logger v (NodeId n) =
    //    match tryCreateRegistrySubKey logger (getNodeSubKey v) with
    //    | Some _ -> trySetRegistryValue logger (getNodeSubKey v) nodeIdKey (n.ToString())
    //    | None -> None


    // Messaging Server
    let tryGetMessagingServerAddress (logger : Logger) v =
        match tryGetRegistryValue logger (getMessagingServerSubKey v) serviceAddressKey with
        | Some s -> ServiceAddress s |> Some
        | None -> None


    let trySetMessagingServerAddress (logger : Logger) v (ServiceAddress a) =
        match tryCreateRegistrySubKey logger (getMessagingServerSubKey v) with
        | Some _ -> trySetRegistryValue logger (getMessagingServerSubKey v) serviceAddressKey a
        | None -> None


    let tryGetMessagingServerPort (logger : Logger) v =
        match tryGetRegistryValue logger (getMessagingServerSubKey v) servicePortKey with
        | Some s ->
            match Int32.TryParse s with
            | true, v -> ServicePort v |> Some
            | false, _ -> None
        | None -> None


    let trySetMessagingServerPort (logger : Logger) v (ServicePort p) =
        match tryCreateRegistrySubKey logger (getMessagingServerSubKey v) with
        | Some _ -> trySetRegistryValue logger (getMessagingServerSubKey v) servicePortKey (p.ToString())
        | None -> None


    // Messaging Client
    let tryGetMessagingClientAddress (logger : Logger) v c =
        match tryGetRegistryValue logger (getMessagingClientSubKey v c) serviceAddressKey with
        | Some s -> ServiceAddress s |> Some
        | None -> None


    let trySetMessagingClientAddress (logger : Logger) v c (ServiceAddress a) =
        match tryCreateRegistrySubKey logger (getMessagingClientSubKey v c) with
        | Some _ -> trySetRegistryValue logger (getMessagingClientSubKey v c) serviceAddressKey a
        | None -> None


    let tryGetMessagingClientPort (logger : Logger) v c =
        match tryGetRegistryValue logger (getMessagingClientSubKey v c) servicePortKey with
        | Some s ->
            match Int32.TryParse s with
            | true, v -> ServicePort v |> Some
            | false, _ -> None
        | None -> None


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


    // Partitioner - Messaging Client Id
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


    // Number Of Cores
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


    // Storage - Messaging Client Id
    let tryGetStorageMessagingClientId (logger : Logger) v c =
        match tryGetRegistryValue logger (getMessagingClientSubKey v c) storageMessagingClientIdKey with
        | Some s ->
            match Guid.TryParse s with
            | true, v -> v |> MessagingClientId |> StorageId |> Some
            | false, _ -> None
        | None -> None


    let trySetStorageMessagingClientId (logger : Logger) v c (StorageId (MessagingClientId i)) =
        match tryCreateRegistrySubKey logger (getMessagingClientSubKey v c) with
        | Some _ -> trySetRegistryValue logger (getMessagingClientSubKey v c) storageMessagingClientIdKey (i.ToString())
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


    let getStorageImpl getter logger version name p =
        match getter p with
        | Some x -> x
        | None ->
            match tryGetStorageMessagingClientId logger version name with
            | Some x -> x
            | None -> defaultStorageId
