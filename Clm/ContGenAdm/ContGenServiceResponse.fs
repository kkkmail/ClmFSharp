namespace ContGenAdm

open System
open ContGenServiceInfo.ServiceInfo

module ContGenServiceResponse =
    type ContGenResponseHandler () =
        let service : IContGenService = Activator.GetObject (typeof<IContGenService>, getServiceUrl()) :?> IContGenService

        member this.contGenService = service
