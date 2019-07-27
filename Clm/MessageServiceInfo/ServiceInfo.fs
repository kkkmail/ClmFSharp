namespace MessagingServiceInfo

open System
open System.Diagnostics
open ClmSys.GeneralData
open System.Threading

module ServiceInfo =

    [<Literal>]
    let ServiceName = "MessagingService"

    [<Literal>]
    let ProgramName = "MessagingService.exe"


    let getServiceUrl (i : ServiceAccessInfo) =
        getServiceUrlImpl i.serviceAddress.value i.servicePort.value ServiceName


    type MessageId =
        | MessageId of Guid

        member this.value = let (MessageId v) = this in v
        static member create() = Guid.NewGuid() |> MessageId


    type MessageDeliveryType =
        | GuaranteedDelivery
        | NonGuaranteedDelivery


    type MessageInfo<'T> =
        {
            recipient : NodeId
            deliveryType : MessageDeliveryType
            messageData : 'T
        }


    type MessageType =
        | IncomingMessage
        | OutgoingMessage


    type Message<'T> =
        {
            messageId : MessageId
            messageInfo : MessageInfo<'T>
            createdOn : DateTime
        }


    //type ContGenConfigParam =
    //    | SetToIdle
    //    | SetToCanGenerate
    //    | RequestShutDown of waitForCompletion : bool
    //    | SetRunLimit of numberOfCores : int
    //    | CancelTask of processId : int
    //    | SetMinUsefulEe of ee : double


    //type IMessageService =
    //    abstract getState : unit -> ContGenRunnerState
    //    abstract loadQueue : unit -> unit
    //    abstract startGenerate : unit -> unit
    //    abstract updateProgress : ProgressUpdateInfo -> unit
    //    abstract configureService : ContGenConfigParam -> unit
    //    abstract runModel : ModelDataId -> ModelCommandLineParam -> unit
