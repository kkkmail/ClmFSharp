namespace ContGenService

open ProgressNotifier.Interfaces
open ContGen.AsyncRun
open ContGen.Runner

module ContGenServiceInfo =

    [<Literal>]
    let ContGenServiceName = "ContGenService"


    type IContGenService =
        abstract notifyOfProgress : ProgressUpdateInfo -> unit

