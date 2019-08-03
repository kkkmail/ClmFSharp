namespace ClmSys

open System
open VersionInfo
open GeneralData
open MessagingData
open Fake.Windows

module Registry =

    let private nodeIdKey = "NodeId"
    let private serviceAddressKey = "ServiceAddress"
    let private servicePortKey = "ServicePort"
    let private messagingClientIdKey = "ClientId"

    let private getTopSubKey (VersionNumber v) = "CLM" + "\\" + v
    let private getNodeSubKey v = (getTopSubKey v) + "\\" + "Node"
    let private getMessagingServerSubKey v = (getTopSubKey v) + "\\" + "Messaging" + "\\" + "Server"
    
    let private getMessagingClientSubKey v c =
        let t = (getTopSubKey v) + "\\" +  "Messaging" + "\\" + "Client"
        match c with
        | Unnamed -> t
        | Named n -> t + "\\" + n


    let private tryCreateRegistrySubKey logger subKey =
        try
            Registry.createRegistrySubKey Registry.HKEYLocalMachine subKey
            Some ()
        with
            | e ->
                logger e
                None


    let private trySetRegistryValue logger subKey key value =
        try
            Registry.setRegistryValue Registry.HKEYLocalMachine subKey key value
            Some ()
        with
            | e ->
                logger e
                None


    let private tryGetRegistryValueNames logger subKey =
        try
            Registry.getRegistryValueNames Registry.HKEYLocalMachine subKey |> Some
        with
            | e ->
                logger e
                None


    let private tryGetRegistryValue logger subKey key =
        try
             Registry.getRegistryValue Registry.HKEYLocalMachine subKey key |> Some
        with
            | e ->
                logger e
                None


    let private valueExistsForKey logger subKey key =
        try
            Registry.valueExistsForKey Registry.HKEYLocalMachine subKey key
        with
            | e ->
                logger e
                false


    let private tryDeleteRegistryValue logger subKey key =
        try
             Registry.deleteRegistryValue Registry.HKEYLocalMachine subKey key |> Some
        with
            | e ->
                logger e
                None


    let private tryDeleteRegistrySubKey logger subKey =
        try
             Registry.deleteRegistrySubKey Registry.HKEYLocalMachine subKey |> Some
        with
            | e ->
                logger e
                None


    let tryGetNodeId logger v =
        match tryGetRegistryValue logger (getNodeSubKey v) nodeIdKey with
        | Some s ->
            match Guid.TryParse s with
            | true, v -> NodeId v |> Some
            | false, _ -> None
        | None -> None


    let trySetNodeId logger v (NodeId n) =
        match tryCreateRegistrySubKey logger (getNodeSubKey v) with
        | Some _ -> trySetRegistryValue logger (getNodeSubKey v) nodeIdKey (n.ToString())
        | None -> None


    // Messsaging Server
    let tryGetMessagingServerAddress logger v =
        match tryGetRegistryValue logger (getMessagingServerSubKey v) serviceAddressKey with
        | Some s -> ServiceAddress s |> Some
        | None -> None


    let trySetMessagingServerAddress logger v (ServiceAddress a) =
        match tryCreateRegistrySubKey logger (getMessagingServerSubKey v) with
        | Some _ -> trySetRegistryValue logger (getMessagingServerSubKey v) serviceAddressKey a
        | None -> None


    let tryGetMessagingServerPort logger v =
        match tryGetRegistryValue logger (getMessagingServerSubKey v) servicePortKey with
        | Some s ->
            match Int32.TryParse s with
            | true, v -> ServicePort v |> Some
            | false, _ -> None
        | None -> None


    let trySetMessagingServerPort logger v (ServicePort p) =
        match tryCreateRegistrySubKey logger (getMessagingServerSubKey v) with
        | Some _ -> trySetRegistryValue logger (getMessagingServerSubKey v) servicePortKey (p.ToString())
        | None -> None


    // Messsaging Client
    let tryGetMessagingClientAddress logger v c =
        match tryGetRegistryValue logger (getMessagingClientSubKey v c) serviceAddressKey with
        | Some s -> ServiceAddress s |> Some
        | None -> None


    let trySetMessagingClientAddress logger v c (ServiceAddress a) =
        match tryCreateRegistrySubKey logger (getMessagingClientSubKey v c) with
        | Some _ -> trySetRegistryValue logger (getMessagingClientSubKey v c) serviceAddressKey a
        | None -> None


    let tryGetMessagingClientPort logger v c =
        match tryGetRegistryValue logger (getMessagingClientSubKey v c) servicePortKey with
        | Some s ->
            match Int32.TryParse s with
            | true, v -> ServicePort v |> Some
            | false, _ -> None
        | None -> None

    let trySetMessagingClientPort logger v c (ServicePort p) =
        match tryCreateRegistrySubKey logger (getMessagingClientSubKey v c) with
        | Some _ -> trySetRegistryValue logger (getMessagingClientSubKey v c) servicePortKey (p.ToString())
        | None -> None


    let tryGetMessagingClientId logger v c =
        match tryGetRegistryValue logger (getMessagingClientSubKey v c) messagingClientIdKey with
        | Some s ->
            match Guid.TryParse s with
            | true, v -> MessagingClientId v |> Some
            | false, _ -> None
        | None -> None


    let trySetMessagingClientId logger v c (MessagingClientId i) =
        match tryCreateRegistrySubKey logger (getMessagingClientSubKey v c) with
        | Some _ -> trySetRegistryValue logger (getMessagingClientSubKey v c) messagingClientIdKey (i.ToString())
        | None -> None
