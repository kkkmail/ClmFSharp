﻿namespace MessagingService

open System
open ClmSys.VersionInfo
open ClmSys.GeneralData
open MessagingServiceInfo.ServiceInfo
open MessagingService.SvcCommandLine
open Messaging.Service
open ServiceProxy.MessagingService

module ServiceImplementation =

    let createServiceImpl i : ClmMessagingService =
        //let a = createRunner (ModelRunnerParam.defaultValue i (RunnerProxy()))

        //// Send startGenerate in case runner stops due to some reason.
        //let eventHandler _ = a.startGenerate()
        //let timer = new System.Timers.Timer(60_000.0)
        //do timer.AutoReset <- true
        //do timer.Elapsed.Add eventHandler
        //do timer.Start()

        //a
        failwith ""


    let mutable serviceAccessInfo = getServiceAccessInfo []


    type MessagingService () =
        inherit MarshalByRefObject()

        let a = createServiceImpl serviceAccessInfo

        let initService () = ()
        do initService ()

        interface IClmMessagingService with
            member __.sendMessage m = a.sendMessage m
            member __.getMessages n = a.getMessages n
            member __.configureService x = a.configureService x
