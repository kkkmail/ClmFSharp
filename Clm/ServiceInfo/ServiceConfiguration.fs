namespace ServiceInfo

module ServiceConfiguration =

    [<Literal>]
    let ServiceName = "ContGenService"

    [<Literal>]
    let ServiceConfigFileName : string = "ContGenService.config"


    type IContGenService =
        abstract x : unit -> unit

