namespace ContGenAdm

open System
open ClmSys.GeneralData
open ContGenServiceInfo.ServiceInfo

module ContGenServiceResponse =

    type ContGenResponseHandler (i : ContGenServiceAccessInfo) =
        do
            printfn "ContGenResponseHandler: i = %A" i

        let service : IContGenService =
            Activator.GetObject (typeof<IContGenService>, i.contGenServiceAccessInfo.serviceUrl) :?> IContGenService

        member __.contGenService = service
