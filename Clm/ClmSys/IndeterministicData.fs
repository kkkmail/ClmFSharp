namespace ClmSys
open System

module IndeterministicData =

    type Seeder =
        | Seeder of (unit -> int)

        member seeder.nextSeed() =
            let (Seeder s) = seeder
            s()

        static member create (seed : int option) =
            let rnd =
                match seed with
                | Some s -> new Random(s)
                | None -> new Random()

            (fun () -> rnd.Next()) |> Seeder
