namespace ClmSys

module VersionInfo =

    /// Increment fractional part by 0.0001, e.g. 1.0000 -> 1.0001 if an updated version can read the previous version format.
    /// Increment integer part, e.g. 1.0000 -> 2.000, when the changes are completely incompatible with previous version.
    let FileStructureVersion = 3.0000m


    /// This is an overall system version.
    [<Literal>]
    let VersionNumber = "3.0.0.0"


    [<Literal>]
    let CopyrightInfo = "GPL v3 - Copyright Konstantin K. Konstantinov and Alisa F. Konstantinova © 2015 - 2019."
