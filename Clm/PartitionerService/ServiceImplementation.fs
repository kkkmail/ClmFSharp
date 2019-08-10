namespace PartitionerService

open System
open ClmSys.MessagingData
open ClmSys.PartitionerData
open ClmSys.TimerEvents
open PartitionerServiceInfo.ServiceInfo
open PartitionerService.SvcCommandLine
//open Messaging.Service
//open MessagingServiceInfo.ServiceProxy

module ServiceImplementation =

    type PartitionerService(i : PartitionerServiceAccessInfo) =

        member this.onTimer() =
            ignore()


    let createServiceImpl i =
        let w = PartitionerService i
        let h = new EventHandler(EventHandlerInfo.defaultValue w.onTimer)
        do h.start()
