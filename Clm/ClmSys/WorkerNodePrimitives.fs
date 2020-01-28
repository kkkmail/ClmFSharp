namespace ClmSys

open MessagingPrimitives

module WorkerNodePrimitives =

    type WorkerNodeId =
        | WorkerNodeId of MessagingClientId

        member this.value = let (WorkerNodeId v) = this in v
        member this.messagingClientId = let (WorkerNodeId v) = this in v
