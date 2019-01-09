namespace ContGenService

open System
open System.IO
open System.Diagnostics

module ServiceImplementation = 

//    // http://fssnip.net/8I/title/Simple-timedexpiry-cache
//    type CacheMessage<'a,'b when 'a : comparison> =
//    | Get of 'a * AsyncReplyChannel<'b option>
//    | Set of 'a * 'b
//    | Del of 'a


//    let cache<'a,'b when 'a : comparison> (timeout : int) : MailboxProcessor<CacheMessage<'a,'b>> =
//       let expiry = TimeSpan.FromMilliseconds (float timeout)
//       let exp = Map.filter (fun _ (_, dt) -> DateTime.Now - dt >= expiry)
//       let newValue k v = Map.add k (v, DateTime.Now)

//       MailboxProcessor.Start(fun inbox ->
//          let rec loop map =
//             async {
//                let! msg = inbox.TryReceive timeout
//                match msg with
//                | Some (Get (key, channel)) ->
//                    match map |> Map.tryFind key with
//                    | Some (v, dt) when DateTime.Now - dt < expiry ->
//                        channel.Reply (Some v)
//                        return! loop map
//                    | _ ->
//                        channel.Reply None
//                        return! loop (Map.remove key map)
//                | Some (Set (key, value)) ->
//                    return! loop (newValue key value map)
//                | Some (Del (key)) -> 
//                    return! loop (Map.remove key map)
//                | None ->
//                    return! loop (exp map)
//             }
//          loop Map.empty
//       )

//    // sample usage:
////    let c<'a,'b when 'a : comparison> : MailboxProcessor<CacheMessage<'a,'b>> = cache<'a,'b> 1500
////    let get k = c.PostAndReply (fun channel -> Get (k, channel))
////    let set k v = c.Post (Set (k, v))
//    // set 5 "test"
//    // printfn "5 => %A" (get 5)
//    // ... wait for 1.5 seconds, expiry will kick in ...
//    // printfn "5 => %A" (get 5)


//    type CachedResponse<'k, 'v, 'r when 'k : comparison> (timeout : int) = 
//        let responseCache = cache<'k, list<'v>> timeout

//        member this.get (k : 'k) (getter : 'v -> 'r) : 'r option = 
//            match responseCache.PostAndReply (fun channel -> Get (k, channel)) with
//            | Some m -> 
//                match m with
//                | [] -> 
//                    responseCache.Post (Del k)
//                    None
//                | h::t -> 
//                    responseCache.Post (Set (k, t))
//                    Some (getter h)
//            | None -> None

//        member this.set (k : 'k) (v : list<'v>) : unit = responseCache.Post (Set (k, v))

//        member this.del (k : 'k) : unit = responseCache.Post (Del k)


    /// Implementation of the service
    type ContGenService () =
        inherit MarshalByRefObject()

        //let engine : DiscoverRankingEngine = DiscoverRankingEngine.Instance
        //let logger : DiscoverLog = DiscoverLog.Instance

        let initService () : unit = ()
        do initService ()

        member this.doSomething() =
            0

//        interface IDiscoverService with
//            member this.getVersion () : Version = 
//                try
//                    (engine :> IDiscoverService).getVersion ()
//                with
//                    | e -> 
//                        logger.logError (e.Message) |> ignore
//                        Version ()  
        
//            member this.getMatches (qa : array<string * string>) : array<Institution> = 
//                try
//                    let matches : array<Institution> = 
//                        engine.respondWithCompleteMatches ConfigurationParams.Default (userProfile.create qa)
////                        engine.respondWithCompleteMatches { ConfigurationParams.Default with ConfigurationParams.useMinMaxNoOfMatches = false } (userProfile.create qa)
//                        |> Array.map (fun i -> i.toInstitution ())

//                    matches
//                with
//                    | e -> 
//                        logger.logError (e.Message) |> ignore
//                        [| |]

//            member this.getMatchesByCollections (qa : array<string * string>, collectionConfigParams : CollectionConfigParams) : array<CollectionResultInfo> = 
//                try
//                    (engine :> IDiscoverService).getMatchesByCollections (qa, collectionConfigParams)
//                with
//                    | e -> 
//                        logger.logError (e.Message) |> ignore
//                        [| |]
                
//            member this.getSqlVersion () : decimal option = 
//                try
//                    (engine :> IDiscoverService).getSqlVersion ()
//                with
//                    | e -> 
//                        logger.logError (e.Message) |> ignore
//                        None 

//            member this.getCollections () : array<Value> =
//                try
//                    (engine :> IDiscoverService).getCollections ()
//                with
//                    | e -> 
//                        logger.logError (e.Message) |> ignore
//                        [| |]                

//            member this.getCollectionsContent (collections : array<string>, collectionConfigParams : CollectionConfigParams) : array<CollectionResultInfo> =
//                try
//                    (engine :> IDiscoverService).getCollectionsContent (collections, collectionConfigParams)
//                with
//                    | e -> 
//                        logger.logError (e.Message) |> ignore
//                        [| |]  

//            member this.getStateSystems (state : string, collectionConfigParams : CollectionConfigParams) : array<CollectionResultInfo> =
//                try
//                    (engine :> IDiscoverService).getStateSystems (state, collectionConfigParams)
//                with
//                    | e -> 
//                        logger.logError (e.Message) |> ignore
//                        [| |]  

//            member this.getMenuData () : string = 
//                try
//                    (engine :> IDiscoverService).getMenuData ()
//                with
//                    | e -> 
//                        logger.logError (e.Message) |> ignore
//                        "<ERROR />"        

//            member this.getMenuVersion () : string = 
//                try
//                    (engine :> IDiscoverService).getMenuVersion ()
//                with
//                    | e -> 
//                        logger.logError (e.Message) |> ignore
//                        ""  

//            member this.getDataSources () : array<string> = 
//                try
//                    (engine :> IDiscoverService).getDataSources ()
//                with
//                    | e -> 
//                        logger.logError (e.Message) |> ignore
//                        [| |]  

//            member this.getDataSource (dsName : string) : array<Value> = 
//                try
//                    (engine :> IDiscoverService).getDataSource dsName
//                with
//                    | e -> 
//                        logger.logError (e.Message) |> ignore
//                        [| |]  
