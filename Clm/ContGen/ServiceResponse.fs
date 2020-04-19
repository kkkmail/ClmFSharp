namespace ContGen

open ContGenServiceInfo.ServiceInfo
open ClmSys.ClmErrors
open ClmSys.ContGenData
open ClmSys.Wcf
open ClmSys.ContGenErrors

module ServiceResponse =

    /// Low level WCF messaging client.
    /// It seems nearly imposible to bake in "tryCommunicate tryGetWcfService" into a single function due to generics + type inference interplay.
    /// It does look very simple. However, tryCommunicate is an implicit generics with 4 generic parameters. Thread carefully...
    type ContGenResponseHandler private (url) =
        let tryGetWcfService() = tryGetWcfService<IContGenWcfService> url
        let tryPeekMsgWcfErr e = e |> TryDeleteRunQueueWcfErr |> TryDeleteRunQueueErr |> ContGenServiceErr
        let tryCancelRunQueueImpl q = tryCommunicate tryGetWcfService (fun service -> service.tryCancelRunQueue) tryPeekMsgWcfErr q

        interface IContGenService with
            member __.tryCancelRunQueue q = tryCancelRunQueueImpl q

        new (i : ContGenServiceAccessInfo) = ContGenResponseHandler(i.wcfServiceUrl)
