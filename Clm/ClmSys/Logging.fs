namespace ClmSys

module Logging =

    type Logger =
        {
            logInfo : string -> unit
            logErr : string -> unit
            logExn : string -> exn -> unit
        }

        static member defaultValue =
            {
                logInfo = fun s -> printfn "%s" s
                logErr = fun s -> printfn "%s" s
                logExn = fun s e -> printfn "%s, exception: %A." s e
            }

        static member ignored =
            {
                logInfo = fun _ -> ignore()
                logErr = fun _ -> ignore()
                logExn = fun _ _ -> ignore()
            }


    let logger = Logger.defaultValue
