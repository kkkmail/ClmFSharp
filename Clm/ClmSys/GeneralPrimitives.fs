namespace ClmSys

open System
open ClmSys.VersionInfo

module GeneralPrimitives =

    let DefaultContGenServiceAddress = "localhost"

    let DefaultWorkerNodeServicePort = 20000 + DefaultContGenServicePort
    let DefaultWorkerNodeServiceAddress = "localhost"

    let DefaultMessagingServerPort = 40000 + DefaultContGenServicePort
    let DefaultMessagingServerAddress = "localhost"


    type ClmDefaultValueId =
        | ClmDefaultValueId of int64

        member df.value = let (ClmDefaultValueId v) = df in v
        override df.ToString() = df.value.ToString().PadLeft(9, '0')


    type ServiceAddress =
        | ServiceAddress of string

        member this.value = let (ServiceAddress v) = this in v
        static member defaultContGenServiceValue = ServiceAddress DefaultContGenServiceAddress
        static member defaultMessagingServerValue = ServiceAddress DefaultMessagingServerAddress
        static member defaultWorkerNodeServiceValue = ServiceAddress DefaultWorkerNodeServiceAddress


    type ServicePort =
        | ServicePort of int

        member this.value = let (ServicePort v) = this in v
        static member defaultContGenServiceValue = ServicePort DefaultContGenServicePort
        static member defaultMessagingServerValue = ServicePort DefaultMessagingServerPort
        static member defaultWorkerNodeServiceValue = ServicePort DefaultWorkerNodeServicePort


    type ConnectionString =
        | ConnectionString of string

        member this.value = let (ConnectionString v) = this in v


    type ResultDataId =
        | ResultDataId of Guid

        member this.value = let (ResultDataId v) = this in v


    type LocalProcessId =
        | LocalProcessId of int

        member this.value = let (LocalProcessId v) = this in v


    type RemoteProcessId =
        | RemoteProcessId of Guid

        member this.value = let (RemoteProcessId v) = this in v
        member this.toResultDataId() = this.value |> ResultDataId


    type RunQueueId =
        | RunQueueId of Guid

        member this.value = let (RunQueueId v) = this in v
        member this.toResultDataId() = this.value |> ResultDataId
        member this.toRemoteProcessId() = this.value |> RemoteProcessId


