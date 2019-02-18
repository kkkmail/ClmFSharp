namespace ClmSys
open System
open System.IO
open System.IO.Compression
open System.Text

module GeneralData =

    /// String.Empty is not a const.
    [<Literal>]
    let EmptyString = ""


    /// Environment.NewLine is too long and it is not a const.
    [<Literal>]
    let Nl = "\r\n"


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
        | ModelDataId of int64

        member this.value = let (ModelDataId v) = this in v


    type ResultDataId =
        | ResultDataId of int64

        member this.value = let (ResultDataId v) = this in v


    type RunQueueId =
        | RunQueueId of int64

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
