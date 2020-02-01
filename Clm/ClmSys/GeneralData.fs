﻿namespace ClmSys

open System
open System.IO
open System.IO.Compression
open System.Text
open ClmSys.VersionInfo
open ClmSys.Logging
open System.Diagnostics
open MBrace.FsPickler
open Newtonsoft.Json
open GeneralPrimitives
open GeneralErrors

module GeneralData =

    [<Literal>]
    let DefaultRootDrive = "C"


    /// String.Empty is not a const.
    [<Literal>]
    let EmptyString = ""


    /// Environment.NewLine is too long and it is not a const.
    [<Literal>]
    let Nl = "\r\n"


    let getVersionImpl getter p =
        match getter p with
        | Some x -> x
        | None -> versionNumberValue


    let toValidServiceName (serviceName : string) =
        serviceName.Replace(" ", "").Replace("-", "").Replace(".", "")


    let private getServiceUrlImpl serviceAddress (servicePort : int) serviceName =
        "tcp://" + serviceAddress + ":" + (servicePort.ToString()) + "/" + serviceName


    let private getWcfServiceUrlImpl serviceAddress (servicePort : int) serviceName =
        "net.tcp://" + serviceAddress + ":" + (servicePort.ToString()) + "/" + serviceName


    type ServiceAccessInfo =
        {
            serviceAddress : ServiceAddress
            servicePort : ServicePort
            inputServiceName : string
        }

        member s.serviceName = s.inputServiceName
        member s.serviceUrl = getServiceUrlImpl s.serviceAddress.value s.servicePort.value s.serviceName
        member s.wcfServiceName = toValidServiceName s.inputServiceName
        member s.wcfServiceUrl = getWcfServiceUrlImpl s.serviceAddress.value s.servicePort.value s.wcfServiceName


    let toVariableName (s : string) =
        match s.Length with
        | 0 -> s
        | 1 -> s.ToLower()
        | _ -> s.Substring(0, 1).ToLower() + s.Substring(1)


    let zip (s : string) =
        let b = Encoding.UTF8.GetBytes(s)
        use i = new MemoryStream(b)
        use o = new MemoryStream()
        use g = new GZipStream(o, CompressionMode.Compress)
        i.CopyTo(g, 4096)
        i.Close()
        g.Close()
        o.Close()
        o.ToArray()


    let unZip (b : byte[]) =
        use i = new MemoryStream(b)
        use g = new GZipStream(i, CompressionMode.Decompress)
        use o = new MemoryStream()
        g.CopyTo(o, 4096)
        g.Close()
        i.Close()
        o.Close()
        let s = Encoding.UTF8.GetString(o.ToArray())
        s


    //let doAsyncTask (f : unit-> 'a) =
    //     async { return! Task<'a>.Factory.StartNew( new Func<'a>(f) ) |> Async.AwaitTask }

    let toAsync (f : unit-> unit) = async { do f() }


    type ResultDataId
        with
        member this.toRunQueueId() = this.value |> RunQueueId


    type RemoteProcessId
        with
        member this.toRunQueueId() = this.value |> RunQueueId


    /// http://www.fssnip.net/q0/title/SystemTimeSpan-userfriendly-formatting
    type Part =
        | Days of int
        | Hours of int
        | Minutes of int
        | Seconds of int
        | Milliseconds of int


    let bigPartString p =
        match p with
        | Days 0 -> EmptyString
        | Days 1 -> "a day"
        | Days d -> sprintf "%i days" d
        | Hours 0 -> EmptyString
        | Hours 1 -> "an hour"
        | Hours h -> sprintf "%i hours" h
        | Minutes 0 -> EmptyString
        | Minutes 1 -> "a minute"
        | Minutes m -> sprintf "%i minutes" m
        | _ -> EmptyString


    let smallPartString s m =
        match s, m with
        | Seconds 0, Milliseconds 0  -> EmptyString
        | Seconds 0, Milliseconds ms -> sprintf "%ims" ms
        | Seconds 1, Milliseconds 0  -> sprintf "a second"
        | Seconds s, Milliseconds 0  -> sprintf "%i seconds" s
        | Seconds s, Milliseconds ms -> sprintf "%i.%i seconds" s ms
        | _                          -> EmptyString


    let formatTimeSpanDetailed maxParts (ts:TimeSpan) =
        let makePart (p, v) = (p v, v)
        let bigParts =
            [
                (Days, ts.Days)
                (Hours, ts.Hours)
                (Minutes, ts.Minutes)
            ]
            |> Seq.map makePart
            |> Seq.skipWhile (snd >> ((>) 0))
  
        let flip f a b = f b a

        bigParts
        |> Seq.map fst
        |> Seq.map bigPartString
        |> flip Seq.append [smallPartString (Seconds ts.Seconds) (Milliseconds ts.Milliseconds)]
        |> Seq.filter (not << String.IsNullOrEmpty)
        |> Seq.truncate maxParts
        |> fun parts -> String.Join(", ", parts)


    let formatTimeSpan (t : TimeSpan) =
        let x = sprintf "%i:%02i:%02i" t.Hours t.Minutes t.Seconds

        if t.Days = 0
        then x
        else sprintf "%i day(s), %s" t.Days x


    type IUpdater<'I, 'A, 'S> = 
        abstract member init : 'I -> 'S
        abstract member add : 'A -> 'S -> 'S


    type Updater<'T> = MailboxProcessor<'T>


    type UpdatatableStorage<'A, 'S> = 
        | AddContent of 'A
        | GetContent of AsyncReplyChannel<'S>


    type AsyncUpdater<'I, 'A, 'S> (updater : IUpdater<'I, 'A, 'S>, i : 'I) =
        let chat = Updater.Start(fun u ->
          let rec loop s = async {
            let! m = u.Receive()

            match m with
            | AddContent a -> return! loop (updater.add a s)
            | GetContent r ->
                r.Reply s
                return! loop s }

          updater.init i |> loop)

        member _.addContent p = AddContent p |> chat.Post
        member _.getContent () = chat.PostAndReply GetContent


    let estimateEndTime progress (started : DateTime) =
        if progress > 0.0m && progress <= 1.0m
        then
            let estRunTime = (decimal (DateTime.Now.Subtract(started).Ticks)) / progress |> int64 |> TimeSpan.FromTicks
            started.Add estRunTime |> Some
        else None


    let partition maxVal q n =
        let (a, b) =
            q
            |> List.mapi (fun i e -> (i + n + 1, e))
            |> List.partition (fun (i, _) -> i <= maxVal)

        (a |> List.map snd, b |> List.map snd)


    type Map<'k, 'v when 'k : comparison>
        with

        /// Tries to remove a given key from the map.
        /// If found, then returns a new map with the key removed.
        /// If not found, then returns the orignial map.
        member m.tryRemove k =
            match m.ContainsKey k with
            | true -> m.Remove k
            | false -> m


        /// Tries to get value out of the map OR returns a given default value if there is none.
        member m.getValueorDefault k v = m |> Map.tryFind k |> Option.defaultValue v


    /// http://www.fssnip.net/1T/title/Remove-first-ocurrence-from-list.
    /// Removes first occurrence from the list when the element matches a given predicate.
    let rec removeFirst pred lst =
        match lst with
        | [] -> []
        | h :: t when pred h -> t
        | h :: t -> h :: removeFirst pred t


    let getExeName exeName =
        // TODO kk:20190208 - This is a fucked up NET way to get what's needed. Refactor when time permits.
        // See:
        //     https://stackoverflow.com/questions/278761/is-there-a-net-framework-method-for-converting-file-uris-to-paths-with-drive-le
        //     https://stackoverflow.com/questions/837488/how-can-i-get-the-applications-path-in-a-net-console-application
        let x = Uri(System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().CodeBase)).LocalPath
        x + @"\" + exeName


    type Queue<'A> =
        | Queue of 'A list * 'A list


    let emptyQueue = Queue([], [])


    let enqueue q e =
        match q with
        | Queue(fs, bs) -> Queue(e :: fs, bs)


    let tryDequeue q =
        match q with
        | Queue([], []) -> None, q
        | Queue(fs, b :: bs) -> Some b, Queue(fs, bs)
        | Queue(fs, []) -> 
            let bs = List.rev fs
            Some bs.Head, Queue([], bs.Tail)


    type List<'T>
        with
        static member mapWhileSome mapper tList =
            let rec doMap x acc =
                match x with
                | [] -> acc
                | h :: t ->
                    match mapper h with
                    | Some u -> doMap t (u :: acc)
                    | None -> acc

            doMap tList []
            |> List.rev


        static member mapWhileSomeAsync mapper tList =
            async {
                let rec doMap x acc =
                    async {
                        match x with
                        | [] -> return acc
                        | h :: t ->
                            match! mapper h with
                            | Some u -> return! doMap t (u :: acc)
                            | None -> return acc
                    }

                let! lst = doMap tList []
                return lst |> List.rev
            }


        static member mapAsync mapper tList =
            async {
                let x = tList |> List.map (fun e -> async { return! mapper e} )
                let rec doMap x acc =
                    async {
                        match x with
                        | [] -> return acc
                        | h :: t ->
                            let! u = mapper h
                            return! doMap t (u :: acc)
                    }

                let! lst = doMap tList []
                return lst |> List.rev
            }


        static member foldWhileSome mapper tList seed =
            //printfn "foldWhileSome: seed = %A" seed

            let rec doFold x acc =
                //printfn "    foldWhileSome: acc = %A" acc
                match x with
                | [] -> acc
                | h :: t ->
                    match mapper acc h with
                    | Some u -> doFold t u
                    | None -> acc

            let y = doFold tList seed
            //printfn "    foldWhileSome - completed: y = %A" y
            y


        static member foldWhileSomeAsync mapper tList seed =
            async {
                //printfn "foldAsyncWhileSome: seed = %A" seed

                let rec doFold x acc =
                    async {
                        //printfn "    foldAsyncWhileSome: acc = %A" acc
                        match x with
                        | [] -> return acc
                        | h :: t ->
                            match! mapper acc h with
                            | Some u -> return! doFold t u
                            | None -> return acc
                    }

                let! y = doFold tList seed
                //printfn "    foldAsyncWhileSome - completed: y = %A" y
                return y
            }

    /// http://www.fssnip.net/iW/title/Oneliner-generic-timing-function
    let time f a = System.Diagnostics.Stopwatch.StartNew() |> (fun sw -> (f a, sw.Elapsed))


    let private timedImpl (l : Logger) name f =
        let (r, t) = time f ()

        if t.TotalSeconds <= 5.0
        then l.logInfoString (sprintf "%s: Execution time: %A" name t)
        else l.logInfoString (sprintf "%s: !!! LARGE Execution time: %A" name, t)

        r

    let timed name f a = timedImpl logger name (fun () -> f a)


    let tryGetProcessById (LocalProcessId v) =
        try
            Process.GetProcessById v |> Some
        with
        | _ -> None


    let tryGetProcessName (p : Process) =
        try
            p.ProcessName |> Some
        with
        | _ -> None


    let xmlSerializer = FsPickler.CreateXmlSerializer(indent = true)
    let xmlSerialize t = xmlSerializer.PickleToString t
    let xmlDeserialize s = xmlSerializer.UnPickleOfString s


    let jsonSerialize t = JsonConvert.SerializeObject t
    let jsonDeserialize<'T> s = JsonConvert.DeserializeObject<'T> s


    let serializer = xmlSerializer
    let serialize t = serializer.PickleToString t
    let deserialize s = serializer.UnPickleOfString s
    //let serialize t = jsonSerialize t
    //let deserialize s = jsonDeserialize s


    let trySerialize<'A> (a : 'A) : Result<byte[], SerializationError> =
        try
            printfn "trySerialize: Serializing type %A..." typeof<'A>
            //printfn "trySerialize: a = '%A'." a
            let b = a |> serialize
            //printfn "trySerialize: b = '%A'." b
            let c = b |> zip
            //printfn "trySerialize: c = '%A'." c
            c |> Ok
        with
        | e ->
            printfn "trySerialize: Exception: '%A'." e
            e |> SerializationException |> Error


    /// https://stackoverflow.com/questions/2361851/c-sharp-and-f-casting-specifically-the-as-keyword
    let tryCastAs<'T> (o:obj) : 'T option =
        //printfn "tryCastAs: typeof<'T> = '%A', o.GetType() = '%A'." typeof<'T> (o.GetType())
        match o with
        | :? 'T as res -> Some res
        | _ -> None


    let tryDeserialize<'A> (b : byte[]) : Result<'A, SerializationError> =
        try
            //printfn "tryDeserialize: Unzipping..."
            let x = b |> unZip
            //printfn "tryDeserialize: x = %A" x

            printfn "tryDeserialize: Deserializing into type %A..." typeof<'A>
            let (y : 'A) = deserialize x
            //printfn "tryDeserialize: y = %A" y
            Ok y
        with
        | e ->
            printfn "tryDeserialize: Exception: '%A'." e
            e |> DeserializationExn |> Error


    /// Replies with result and returns the state.
    /// It is used by MailboxProcessor based classes to standardize approach for PostAndReply.
    let withReply (r : AsyncReplyChannel<'T>) (s, result) =
        r.Reply result
        s
