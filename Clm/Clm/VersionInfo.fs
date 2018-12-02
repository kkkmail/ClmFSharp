namespace Clm

module VersionInfo =

    /// !!! Changing FileStructureVersionNumber makes ALL previously generated code unusable !!!
    /// It must be then either manually updated OR just set aside and used with the relevant code, which matches its FileStructureVersionNumber.
    [<Literal>]
    let FileStructureVersionNumber = "1.0.0.0"


    /// This is a version of the code. Update as necessary but respect FileStructureVersionNumber.
    [<Literal>]
    let VersionNumber = "1.0.1.1"
