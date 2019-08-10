namespace WorkerNodeService

open System
open ClmSys.MessagingData
open ClmSys.WorkerNodeData
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.SvcCommandLine
//open Messaging.Service
open MessagingServiceInfo.ServiceProxy

module ServiceImplementation =

    type WorkerNodeService =
        | Dummy


    let createServiceImpl (i : WorkerNodeServiceAccessInfo) : WorkerNodeService =
        //let d : MessagingServiceData =
        //    {
        //        messagingServiceProxy = MessagingServiceProxy.defaultValue
        //    }

        //let a = MessagingService d

        ////let eventHandler _ = a.startGenerate()
        ////let timer = new System.Timers.Timer(60_000.0)
        ////do timer.AutoReset <- true
        ////do timer.Elapsed.Add eventHandler
        ////do timer.Start()

        //a
        failwith ""


    let mutable serviceAccessInfo = getServiceAccessInfo []


    type WrkNodeService () =
        //inherit MarshalByRefObject()

        let a = createServiceImpl serviceAccessInfo

        let initService () = ()
        do initService ()

        //interface IMessagingService with
        //    member __.sendMessage m = a.sendMessage m
        //    member __.getMessages n = a.getMessages n
        //    member __.configureService x = a.configureService x

        member __.doSomething() =
            failwith ""
