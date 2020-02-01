namespace ContGen

open System
open System.Diagnostics
open ClmSys.GeneralData
open Clm.ModelParams
open ContGenServiceInfo.ServiceInfo
open ClmSys.AsyncRunErrors
open ClmSys.ClmErrors
open ClmSys.ContGenPrimitives
open ClmSys.GeneralPrimitives
open ClmSys.ModelGeneratorErrors
open ServiceProxy.ModelGeneratorProxy

module ModelGeneration =

    let private toError g f = f |> g |> ModelGeneratorErr |> Error
    let private addError g f e = ((f |> g |> ModelGeneratorErr) + e) |> Error


    type ModelGeneratorData =
        {
            modelGeneratorProxy : ModelGeneratorProxy
        }


    ////type ModelGeneratorWorkState =
    ////    | NotGeneratingModels
    ////    | GeneratingModels


    type ModelGeneratorState =
        {
            //modelGeneratorWorkState : ModelGeneratorWorkState
            modelGeneratorWorkState : int // dummy
        }

        static member defaultValue =
            {
                //modelGeneratorWorkState = NotGeneratingModels
                modelGeneratorWorkState = 0
            }


    type ModelGeneratorMessage =
        | GenerateModels of AsyncReplyChannel<UnitResult>


    type OnGenerateModelsProxy =
        {
            generate : unit -> ListResult<RunQueueId>
        }


    let onGenerateModels (proxy : OnGenerateModelsProxy) (s : ModelGeneratorState) =
        //match s.modelGeneratorWorkState with
        //| NotGeneratingModels ->
        //    s, Ok()
        //| GeneratingModels -> s, Ok()
        let results = proxy.generate()
        s, proxy.generate()


    type ModelGenerator (i : ModelGeneratorData) =
        let onGenerateModelsProxy : OnGenerateModelsProxy = 0

        let messageLoop =
            MailboxProcessor.Start(fun u ->
                let rec loop s =
                    async
                        {
                            match! u.Receive() with
                            | GenerateModels r -> return! onGenerateModels onGenerateModelsProxy s |> (withReply r) |> loop
                        }

                ModelGeneratorState.defaultValue |> loop
                )

        member _.generateModels() = messageLoop.PostAndReply GenerateModels
