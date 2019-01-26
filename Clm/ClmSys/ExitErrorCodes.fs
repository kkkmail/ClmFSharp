namespace ClmSys

module ExitErrorCodes =

    [<Literal>]
    let CompletedSuccessfully = 0

    [<Literal>]
    let InvalidCommandLineArgs = -1

    [<Literal>]
    let UnknownException = -2

    [<Literal>]
    let DatabaseErrorOccurred = -3
