open System.Collections.Generic

let values = [0..1_000_000] |> List.map (fun n -> (n, n * n))

printfn "Build Map"
#time
let map = values |> List.fold (fun map (key, value) -> map |> Map.add key value) Map.empty
#time

let dic = new Dictionary<int, int>()

printfn "Build Dictionary"
#time
values |> List.iter (fun v -> dic.Add v)
#time

//let dic =
//    values |> List.scan (fun last kvAdd -> let next = new Dictionary()
//    [for (kvPair:KeyValuePair) in last do next.Add(kvPair.Key, kvPair.Value)] |> ignore;
//    next ) (new Dictionary())
#time

