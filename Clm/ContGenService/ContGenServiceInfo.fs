namespace ContGenService

open ProgressNotifier.Interfaces
open ContGen.AsyncRun
open ContGen.Runner

module ContGenServiceInfo =

    [<Literal>]
    let ContGenServiceName = "ContGenService"

    [<Literal>]
    let ProgramName = "ContGenService.exe"


    type IContGenService =
        abstract notifyOfProgress : ProgressUpdateInfo -> unit

