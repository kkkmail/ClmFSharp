namespace ClmSys

open System
open System.Diagnostics

module Logging =

    type LogInfo =
        {
            Message : string
            Date : DateTime
        }


    type ErrorInfo =
        {
            Error : exn
            StackTrace : StackTrace
        }


    type LogMessage =
        | Debug of LogInfo
        | Info of LogInfo
        | Warning of LogInfo
        | Error of ErrorInfo * LogInfo
        | Fatal of ErrorInfo * LogInfo

        member this.Message =
            match this with | Debug i | Info i | Warning i | Error (_, i) | Fatal (_, i) -> i.Message
        member this.Exception =
            match this with | Error (e, _) | Fatal (e, _) -> Some e.Error | _ -> None
        member this.Level =
            match this with | Debug _ -> Level.Debug | Info _ -> Level.Info | Warning _ -> Level.Warn | Error _ -> Level.Error | Fatal _ -> Level.Fatal
        member this.LogInfo =
            match this with | Debug i | Info i | Warning i | Error (_, i) | Fatal (_, i) -> i


    let logAgent = MailboxProcessor.Start <| fun inbox ->
        let rec logLoop () = async {
            let! (message : LogMessage) = inbox.Receive()
            writeLog message.Level message.LogInfo.Message message.Exception message.LogInfo.Date message.LogInfo.StackTrace
            return! logLoop()
        }
        logLoop ()


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
