namespace ClmSys

open System
open VersionInfo
open GeneralData
open MessagingData
open Fake.Windows

module Registry =

    [<Literal>]
    let TopSubKey = "CLM" + "\\" + VersionNumberValue

    [<Literal>]
    let NodeSubKey = "Node"

    [<Literal>]
    let NodeSubKeyFull = TopSubKey + "\\" + NodeSubKey

    [<Literal>]
    let NodeIdKey = "NodeId"

    [<Literal>]
    let MessagingSubKey = "Messaging"

    [<Literal>]
    let MessagingServerSubKeyFull = TopSubKey + "\\" + MessagingSubKey + "\\" + "Server"

    [<Literal>]
    let MessagingClientSubKeyFull = TopSubKey + "\\" + MessagingSubKey + "\\" + "Client"

    [<Literal>]
    let MessagingServiceAddressKey = "ServiceAddress"

    [<Literal>]
    let MessagingServicePortKey = "ServicePort"

    [<Literal>]
    let MessagingClientIdKey = "ClientId"


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


    let tryGetNodeId logger =
        match tryGetRegistryValue logger NodeSubKeyFull NodeIdKey with
        | Some s ->
            match Guid.TryParse s with
            | true, v -> NodeId v |> Some
            | false, _ -> None
        | None -> None


    let trySetNodeId logger (NodeId n) =
        match tryCreateRegistrySubKey logger NodeSubKeyFull with
        | Some _ -> trySetRegistryValue logger NodeSubKeyFull NodeIdKey (n.ToString())
        | None -> None


    // Messsaging Server
    let tryGetMessagingServerAddress logger =
        match tryGetRegistryValue logger MessagingServerSubKeyFull MessagingServiceAddressKey with
        | Some s -> ServiceAddress s |> Some
        | None -> None


    let trySetMessagingServerAddress logger (ServiceAddress a) =
        match tryCreateRegistrySubKey logger MessagingServerSubKeyFull with
        | Some _ -> trySetRegistryValue logger MessagingServerSubKeyFull MessagingServiceAddressKey a
        | None -> None


    let tryGetMessagingServerPort logger =
        match tryGetRegistryValue logger MessagingServerSubKeyFull MessagingServicePortKey with
        | Some s ->
            match Int32.TryParse s with
            | true, v -> ServicePort v |> Some
            | false, _ -> None
        | None -> None

    let trySetMessagingServerPort logger (ServicePort p) =
        match tryCreateRegistrySubKey logger MessagingServerSubKeyFull with
        | Some _ -> trySetRegistryValue logger MessagingServerSubKeyFull MessagingServicePortKey (p.ToString())
        | None -> None


    let private getMessagingClientSubKey c =
        match c with
        | Unnamed -> MessagingServerSubKeyFull
        | Named n -> MessagingServerSubKeyFull + "\\" + n


    // Messsaging Client
    let tryGetMessagingClientAddress logger c =
        match tryGetRegistryValue logger (getMessagingClientSubKey c) MessagingServiceAddressKey with
        | Some s -> ServiceAddress s |> Some
        | None -> None


    let trySetMessagingClientAddress logger c (ServiceAddress a) =
        match tryCreateRegistrySubKey logger (getMessagingClientSubKey c) with
        | Some _ -> trySetRegistryValue logger (getMessagingClientSubKey c) MessagingServiceAddressKey a
        | None -> None


    let tryGetMessagingClientPort logger c =
        match tryGetRegistryValue logger (getMessagingClientSubKey c) MessagingServicePortKey with
        | Some s ->
            match Int32.TryParse s with
            | true, v -> ServicePort v |> Some
            | false, _ -> None
        | None -> None

    let trySetMessagingClientPort logger c (ServicePort p) =
        match tryCreateRegistrySubKey logger (getMessagingClientSubKey c) with
        | Some _ -> trySetRegistryValue logger (getMessagingClientSubKey c) MessagingServicePortKey (p.ToString())
        | None -> None


    let tryGetMessagingClientId logger c =
        match tryGetRegistryValue logger (getMessagingClientSubKey c) MessagingClientIdKey with
        | Some s ->
            match Guid.TryParse s with
            | true, v -> MessagingClientId v |> Some
            | false, _ -> None
        | None -> None


    let trySetMessagingClientId logger c (MessagingClientId i) =
        match tryCreateRegistrySubKey logger (getMessagingClientSubKey c) with
        | Some _ -> trySetRegistryValue logger (getMessagingClientSubKey c) MessagingClientIdKey (i.ToString())
        | None -> None
