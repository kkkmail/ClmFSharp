﻿namespace ClmSys

module VersionInfo =

    /// Increment fractional part by 0.0001, e.g. 1.0000 -> 1.0001 if an updated version can read the previous version format.
    /// Increment integer part and reset fractional part, e.g. 1.0023 -> 2.0000, when the changes are completely incompatible with previous version.
    let FileStructureVersion = 3.0000m


    /// This is an overall system version.
    [<Literal>]
    let VersionNumberValue = "4.0.0.0"


    /// This is the name of the system used, for example, to access the Registry.
    [<Literal>]
    let SystemName = "CLM"


    [<Literal>]
    let CopyrightInfo = "GPL v3 - Copyright Konstantin K. Konstantinov and Alisa F. Konstantinova © 2015 - 2019."


    type VersionNumber =
        | VersionNumber of string

        member this.value = let (VersionNumber v) = this in v


    let versionNumberValue = VersionNumber VersionNumberValue


    type MessagingDataVersion =
        | MessagingDataVersion of int

        member this.value = let (MessagingDataVersion v) = this in v

    /// Increment when internal messaging structures change and
    /// messages can no longer be successfully transferred among components.
    let messagingDataVersion = MessagingDataVersion 2
