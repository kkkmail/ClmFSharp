namespace ContGenAdm

open System
open ClmSys.GeneralData
open ContGenServiceInfo.ServiceInfo

module ContGenServiceResponse =

    type ContGenResponseHandler (i : ServiceAccessInfo) =
        let service : IContGenService =
            Activator.GetObject (typeof<IContGenService>, getServiceUrl i) :?> IContGenService

        member this.contGenService = service
