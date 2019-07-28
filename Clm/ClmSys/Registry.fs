namespace ClmSys
open System
open VersionInfo
open GeneralData
open Fake.Windows

module Registry =

    [<Literal>]
    let TopSubKey = "CLM"

    [<Literal>]
    let NodeSubKey = "Node"

    [<Literal>]
    let NodeSubKeyFull = TopSubKey + "\\" + NodeSubKey + "\\" + VersionNumber

    [<Literal>]
    let NodeIdKey = "NodeId"

    [<Literal>]
    let MessagingSubKey = "Messaging"

    [<Literal>]
    let MessagingSubKeyFull = TopSubKey + "\\" + MessagingSubKey + "\\" + VersionNumber

    [<Literal>]
    let MessagingServerAddressKey = "MessagingServerAddress"

    [<Literal>]
    let MessagingServerPortKey = "MessagingServerPort"


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


    let tryGetMessagingServerAddress logger =
        match tryGetRegistryValue logger MessagingSubKeyFull MessagingServerAddressKey with
        | Some s -> ServiceAddress s |> Some
        | None -> None


    let trySetMessagingServerAddress logger (ServiceAddress a) =
        match tryCreateRegistrySubKey logger MessagingSubKeyFull with
        | Some _ -> trySetRegistryValue logger MessagingSubKeyFull MessagingServerAddressKey a
        | None -> None


    let tryGetMessagingServerPort logger =
        match tryGetRegistryValue logger MessagingSubKeyFull MessagingServerPortKey with
        | Some s ->
            match Int32.TryParse s with
            | true, v -> ServicePort v |> Some
            | false, _ -> None
        | None -> None

    let trySetMessagingServerPort logger (ServicePort p) =
        match tryCreateRegistrySubKey logger MessagingSubKeyFull with
        | Some _ -> trySetRegistryValue logger MessagingSubKeyFull MessagingServerPortKey (p.ToString())
        | None -> None
