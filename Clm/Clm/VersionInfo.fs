﻿namespace Clm

module VersionInfo =

    /// !!! Changing FileStructureVersionNumber is expected to make ALL previously generated code unusable !!!
    /// It must then be either manually updated OR just set aside and used with the relevant code, which matches its FileStructureVersionNumber.
    ///
    /// Update build number, e.g. "1.0.0.*", when the changes are cosmetic, like changing namespace(s) / open declarations 
    ///     OR if changes will be automatically picked up by compiler type inference.
    ///
    /// Update revision number, e.g. "1.0.*.0", when the changes are small and the affected files can be easily updated by hands.
    ///     A single change to DU would qualify as revision update.
    ///
    /// Update minor number, e.g. "1.*.0.0", when the changes are manageable but painful.
    ///     For example, adding an extra layer for multpile DUs, which requires updating all affected DUs, qualifies as minor upgrade.
    ///
    /// Update major version, e.g. "*.0.0.0", when the changes are completely incompatible with previous version.
    ///     Subsequently, it is not possible to update affected files at all (due to lack of information) and / or without extreme efforts.
    [<Literal>]
    let FileStructureVersionNumber = "1.1.0.0"


    /// This is a version of the Code Generator.
    /// It should be the same or higher than FileStructureVersionNumber.
    [<Literal>]
    let VersionNumber = "1.1.1.0"
