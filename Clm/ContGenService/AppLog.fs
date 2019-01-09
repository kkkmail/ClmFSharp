namespace DiscoverService
open System
open System.Diagnostics

open DiscoverInterface.Configuration
open DiscoverInterface.Serialization
open DiscoverInterface.Service

module AppLog = 

    // Type to control logging
    // Can be rewritten when needed without changing the interface
    type DiscoverLogParams = 
        {
            dummy : bool
        }
        with
            static member Default : DiscoverLogParams = 
                {
                    dummy = true
                }


    // Wrapper aroung all internal logging. 
    // Can be rewritten to use log4net or anything else without changing the interface.
    type DiscoverLog (logParams : DiscoverLogParams) = 
        let initLog () : EventLog = 
            // Define the Event Log
            let eventLog : EventLog = new EventLog();
            let eventSource : string = DiscoverServiceName

            if not (EventLog.SourceExists(eventSource)) then EventLog.CreateEventSource(eventSource, "Application");

            eventLog.Source <- eventSource;
            eventLog.Log <- "Application";

            eventLog

        let eventLog : EventLog = initLog ()

        static let instance :  DiscoverLog = DiscoverLog (DiscoverLogParams.Default)

        static member Instance : DiscoverLog = instance

        member this.logEvent (msg : string) : bool = 
            try
                eventLog.WriteEntry (DiscoverServiceName + "::" + msg)
                true
            with
                | e -> false // Logging failed

        member this.logError (errMsg : string, errCode : int option) : bool = 
            match errCode with 
            | Some e -> this.logEvent ("ERROR = " + e.ToString() + ", error message = " + errMsg)
            | None -> this.logEvent ("ERROR = " + "N/A" + ", error message = " + errMsg)

        member this.logError (errMsg : string) : bool = 
            this.logError (errMsg, None)
            


