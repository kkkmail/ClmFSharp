namespace ContGen

open ContGenServiceInfo.ServiceInfo
open ClmSys.ClmErrors
open ClmSys.ContGenData
open ClmSys.Wcf
open ClmSys.ContGenErrors

module ContGenServiceResponse =

    /// Low level WCF messaging client.
    type ContGenResponseHandler private (url) =
        let tryGetWcfService() = tryGetWcfService<IContGenWcfService> url
        let tryPeekMsgWcfErr e = e |> TryDeleteRunQueueWcfErr |> TryDeleteRunQueueErr |> ContGenServiceErr
        let tryCancelRunQueueImpl q = tryCommunicate tryGetWcfService (fun service -> service.tryCancelRunQueue) tryPeekMsgWcfErr q

        interface IContGenService with
            member _.tryCancelRunQueue q = tryCancelRunQueueImpl q

        new (i : ContGenServiceAccessInfo) = ContGenResponseHandler(i.wcfServiceUrl)
