open System.IO
open System.IO.Compression
open System.Text

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
    Encoding.UTF8.GetString(o.ToArray())


let s = [ for i in 0..1000 -> i ] |> List.map (fun i -> "String # " + i.ToString()) |> String.concat ", "
let z = zip s
let u = unZip z

printfn "s.Length = %A" s.Length
printfn "z.Length = %A" z.Length
printfn "(s = u) = %A" (s = u)
