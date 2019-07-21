namespace ClmSys

open System
open System.IO
open System.IO.Compression
open System.Text

module GeneralData =

    /// A base name, which controls the database name and a working folder name.
    /// It is loosely the same as the version number.
    /// It must be updated when the old version is still running (for days) but the new version needs to be deployed.
    /// Eventually it could be bound to the version number, but not today.
    [<Literal>]
    let ClmBaseName = "clm4000"

    [<Literal>]
    let DefaultRootDrive = "C"

    [<Literal>]
    let DefaultContGenServiceAddress = "localhost"

    /// Ideally it should match the numeric part in ClmBaseName to ensure that a new version and an old version can coexist while
    /// the old verison is finishing its run.
    [<Literal>]
    let DefaultContGenServicePort = 4000

    /// String.Empty is not a const.
    [<Literal>]
    let EmptyString = ""

    /// Environment.NewLine is too long and it is not a const.
    [<Literal>]
    let Nl = "\r\n"

    [<Literal>]
    let DefaultMinEe = 0.000_1


    type ServiceAddress =
        | ServiceAddress of string

        member this.value = let (ServiceAddress v) = this in v
        static member defaultValue = ServiceAddress DefaultContGenServiceAddress


    type ServicePort =
        | ServicePort of int

        member this.value = let (ServicePort v) = this in v
        static member defaultValue = ServicePort DefaultContGenServicePort


    type MinUsefulEe =
        | MinUsefulEe of double

        member this.value = let (MinUsefulEe v) = this in v
        static member defaultValue = MinUsefulEe DefaultMinEe


    type ServiceAccessInfo =
        {
            serviceAddress : ServiceAddress
            servicePort : ServicePort
            minUsefulEe : MinUsefulEe
        }


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


    type ConnectionString =
        | ConnectionString of string

        member this.value = let (ConnectionString v) = this in v


    type ModelDataId =
        | ModelDataId of Guid

        member this.value = let (ModelDataId v) = this in v


    type ResultDataId =
        | ResultDataId of Guid

        member this.value = let (ResultDataId v) = this in v


    type RunQueueId =
        | RunQueueId of Guid

        member this.value = let (RunQueueId v) = this in v


    /// http://www.fssnip.net/q0/title/SystemTimeSpan-userfriendly-formatting
    type Part =
        Days of int
            | Hours of int
            | Minutes of int
            | Seconds of int
            | Milliseconds of int


    let bigPartString p =
        match p with
        | Days 0 -> ""
        | Days 1 -> "a day"
        | Days d -> sprintf "%i days" d
        | Hours 0 -> ""
        | Hours 1 -> "an hour"
        | Hours h -> sprintf "%i hours" h
        | Minutes 0 -> ""
        | Minutes 1 -> "a minute"
        | Minutes m -> sprintf "%i minutes" m
        | _ -> ""


    let smallPartString s m =
        match s, m with
        | Seconds 0, Milliseconds 0  -> ""
        | Seconds 0, Milliseconds ms -> sprintf "%ims" ms
        | Seconds 1, Milliseconds 0  -> sprintf "a second"
        | Seconds s, Milliseconds 0  -> sprintf "%i seconds" s
        | Seconds s, Milliseconds ms -> sprintf "%i.%i seconds" s ms
        | _                          -> ""


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

        member this.addContent p = AddContent p |> chat.Post
        member this.getContent () = chat.PostAndReply GetContent


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

        member m.tryRemove k =
            match m.ContainsKey k with
            | true -> m.Remove k
            | false -> m


    //http://www.fssnip.net/1T/title/Remove-first-ocurrence-from-list
    let rec removeFirst pred lst =
        match lst with
        | h::t when pred h -> t
        | h::t -> h::removeFirst pred t
        | _ -> []


    let getExeName exeName =
        // TODO kk:20190208 - This is a fucked up NET way to get what's needed. Refactor when time permits.
        // See:
        //     https://stackoverflow.com/questions/278761/is-there-a-net-framework-method-for-converting-file-uris-to-paths-with-drive-le
        //     https://stackoverflow.com/questions/837488/how-can-i-get-the-applications-path-in-a-net-console-application
        let x = Uri(System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().CodeBase)).LocalPath
        x + @"\" + exeName


    type PartitionerAccessInfo =
        {
            partitionerAddress : ServiceAddress
            partitionerPort : ServicePort
        }
