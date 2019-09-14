namespace ClmSys

open System
open System.Diagnostics
open log4net
open log4net.Core
open System.Reflection

module Logging =

    let logName = "ClmLog"

    //do
    //    printfn "Configuring log4net..."
    //    log4net.Config.XmlConfigurator.Configure()
    //    |> ignore
    //    printfn ".. done."


    let private log4netLogger = LogManager.GetLogger(logName)

    /// https://codereview.stackexchange.com/questions/202745/f-idiomatic-log4net-wrapper
    /// https://stackoverflow.com/questions/9805281/writing-logs-to-file
    /// https://codeshare.co.uk/blog/how-to-set-up-and-configure-error-logging-in-net-with-log4net/
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


    //let private writeLog level message maybeEx logDate (stackTrace: StackTrace) =
    //    let user = Threading.Thread.CurrentPrincipal.Identity
    //    let topFrame = stackTrace.GetFrame(0)
    //    let callingMethod = topFrame.GetMethod()
    //    let location = LocationInfo(callingMethod.DeclaringType.FullName, callingMethod.Name, callingMethod.DeclaringType.Name, String.Empty)

    //    // Correctly populate the read-only Location.StackFrames property by reflectively assigning the underlying m_stackFrames member.
    //    // In .NET, private readonly fields can still be mutated at run-time.  This is not the ideal solution and may be implementaiton-specific,
    //    // but for now, this should work, and it's the only way to get the Stack Trace information into the log message when constructing
    //    // the LoggingEvent manually from F# (since F# does not fully support the CallerMemberName attribute that log4net uses in C#).
    //    typeof<LocationInfo>
    //        .GetField("m_stackFrames", BindingFlags.Instance ||| BindingFlags.NonPublic)
    //        .SetValue(location, stackTrace.GetFrames() |> Array.map StackFrameItem)

    //    match maybeEx with
    //    | Some ex ->
    //        let logData = new LoggingEventData(Domain = AppDomain.CurrentDomain.FriendlyName, Level = level, LocationInfo = location, Message = message, TimeStamp = logDate, LoggerName = "Logger", Identity = user.Name, UserName = user.Name, ExceptionString = ex.ToString())
    //        let logEvent = new LoggingEvent(logData)
    //        log4netLogger.Log(logEvent)
    //    | None -> 
    //        let logData = new LoggingEventData(Domain = AppDomain.CurrentDomain.FriendlyName, Level = level, LocationInfo = location, Message = message, TimeStamp = logDate, LoggerName = "Logger", Identity = user.Name, UserName = user.Name)
    //        let logEvent = new LoggingEvent(logData)
    //        log4netLogger.Log(logEvent)



    let logAgent = MailboxProcessor.Start <| fun inbox ->
        let rec logLoop () = async {
            let! (message : LogMessage) = inbox.Receive()
            printfn "logAgent - logging message: %A" message
            //writeLog message.Level message.LogInfo.Message message.Exception message.LogInfo.Date message.LogInfo.StackTrace
            let logData = new LoggingEventData(Domain = AppDomain.CurrentDomain.FriendlyName, Level = message.Level, Message = message.LogInfo.Message, TimeStamp = message.LogInfo.Date, LoggerName = logName)
            let logEvent = new LoggingEvent(logData)
            log4netLogger.Logger.Log logEvent
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

                //logInfo =
                //    fun s ->
                //        {
                //            Message = s
                //            Date = DateTime.Now
                //        }
                //        |> Info
                //        |> logAgent.Post

                //logErr =
                //    fun s ->
                //        {
                //            Message = s
                //            Date = DateTime.Now
                //        }
                //        |> Info
                //        |> logAgent.Post

                //logExn =
                //    fun s e ->
                //        {
                //            Message = s + ", exception: " + e.ToString()
                //            Date = DateTime.Now
                //        }
                //        |> Info
                //        |> logAgent.Post
            }

        static member ignored = Logger.defaultValue
            //{
            //    logInfo = fun _ -> ignore()
            //    logErr = fun _ -> ignore()
            //    logExn = fun _ _ -> ignore()
            //}


    let logger = Logger.defaultValue
