﻿namespace ProgressNotifierClient

open System
open ClmSys.GeneralData
open ContGenServiceInfo.ServiceInfo

module ServiceResponse =

    type ResponseHandler (i : ContGenServiceAccessInfo) =
        let service = Activator.GetObject (typeof<IContGenService>, getServiceUrl i.serviceAccessInfo) :?> IContGenService

        member this.progressNotifierService : IContGenService = service

        static member tryCreate i =
            try
                ResponseHandler i |> Some
            with
                | exn ->
                    printfn "Exception occurred: %s." exn.Message
                    None
