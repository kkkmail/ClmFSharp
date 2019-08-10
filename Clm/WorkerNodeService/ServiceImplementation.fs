namespace WorkerNodeService

open System
open ClmSys.MessagingData
open ClmSys.WorkerNodeData
open ClmSys.TimerEvents
open WorkerNodeServiceInfo.ServiceInfo
open WorkerNodeService.SvcCommandLine
open MessagingServiceInfo.ServiceProxy

module ServiceImplementation =

    type WorkerNodeMessage =
        | StartWorkerNode
        | Register
        | Unregister


    type WorkerNodeService(i : WorkerNodeServiceAccessInfo) =

        member this.onTimer() =
            ignore()


    let createServiceImpl i =
        let w = WorkerNodeService i
        let h = new EventHandler(EventHandlerInfo.defaultValue w.onTimer)
        do h.start()
