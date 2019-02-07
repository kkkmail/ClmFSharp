namespace ClmSys
open System
open System.IO
open System.IO.Compression
open System.Text
open System.Threading.Tasks

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


    type Seeder =
        | Seeder of (unit -> int)

        static member create (seed : int option) =
            let rnd =
                match seed with
                | Some s -> new Random(s)
                | None -> new Random()

            (fun () -> rnd.Next()) |> Seeder


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
