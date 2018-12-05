namespace Clm

module VersionInfo =

    /// !!! Changing FileStructureVersionNumber is expected to make ALL previously generated code unusable !!!
    /// It must then be either manually updated OR just set aside and used with the relevant code, which matches its FileStructureVersionNumber.
    [<Literal>]
    let FileStructureVersionNumber = "1.0.0.0"


    /// This is a version of the code.
    [<Literal>]
    let VersionNumber = "1.0.2.0"
