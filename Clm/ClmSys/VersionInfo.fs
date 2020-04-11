namespace ClmSys

module VersionInfo =

    type MessagingDataVersion =
        | MessagingDataVersion of int

        member this.value = let (MessagingDataVersion v) = this in v

    /// Increment when:
    ///     1. Internal messaging structures change and messages can no longer be successfully transferred among components.
    ///     2. Some other updates were performed and we need to inform worker nodes that they need to upgrade.
    ///     3. Version number (below) is increased.
    let messagingDataVersion = MessagingDataVersion 78


    /// This is an overall system version.
    [<Literal>]
    let VersionNumberValue = "5.0.1"


    /// A base name, which controls the database name and a working folder name.
    /// It is loosely the same as the version number.
    /// It must be updated when the old version is still running (for days) but the new version needs to be deployed.
    [<Literal>]
    let ClmBaseName = "clm501"


    [<Literal>]
    let MsgSvcBaseName = "msg501"


    /// Default port on which messaging communication is performed.
    let DefaultServicePort = 5000 + messagingDataVersion.value


    /// Increment fractional part by 0.0001, e.g. 1.0000 -> 1.0001 if an updated version can read the previous version format.
    /// Increment integer part and reset fractional part, e.g. 1.0023 -> 2.0000, when the changes are completely incompatible with previous version.
    let FileStructureVersion = 3.1000m


    /// This is the name of the system. It is used, for example, to access Windows Registry.
    [<Literal>]
    let SystemName = "CLM"


    [<Literal>]
    let CopyrightInfo = "GPL v3 (or later vesion) - Copyright Konstantin K. Konstantinov and Alisa F. Konstantinova © 2015 - 2020."


    type VersionNumber =
        | VersionNumber of string

        member this.value = let (VersionNumber v) = this in v


    let versionNumberValue = VersionNumber VersionNumberValue
