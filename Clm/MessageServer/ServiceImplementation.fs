namespace MessagingServer

open System
open ClmSys.GeneralData
open MessagingServiceInfo.ServiceInfo
open Messaging.Server
open ServiceProxy.MessagingServer

module ServiceImplementation =

    let createServiceImpl i : ClmMessagingServer =
        //let a = createRunner (ModelRunnerParam.defaultValue i (RunnerProxy()))

        //// Send startGenerate in case runner stops due to some reason.
        //let eventHandler _ = a.startGenerate()
        //let timer = new System.Timers.Timer(60_000.0)
        //do timer.AutoReset <- true
        //do timer.Elapsed.Add eventHandler
        //do timer.Start()

        //a
        failwith ""

    let mutable serviceAccessInfo =
        {
            messagingServerAccessInfo =
                {
                    serviceAddress = ServiceAddress DefaultMessagingServerAddress
                    servicePort = ServicePort DefaultMessagingServerPort
                }
        }


    type MessagingServer () =
        inherit MarshalByRefObject()

        let a = createServiceImpl serviceAccessInfo

        let initService () = ()
        do initService ()

        interface IClmMessagingServer with
            member __.sendMessage m = a.sendMessage m
            member __.getMessages n = a.getMessages n
