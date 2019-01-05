namespace Clm
open Argu

module CommandLine =

    type CLIArguments =
        | Working_Directory of path:string
        | Listener of host:string * port:int
        | Data of base64:byte[]
        | Port of tcp_port:int
        | Log_Level of level:int
        | Detach
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Working_Directory _ -> "specify a working directory."
                | Listener _ -> "specify a listener (hostname : port)."
                | Data _ -> "binary data in base64 encoding."
                | Port _ -> "specify a primary port."
                | Log_Level _ -> "set the log level."
                | Detach _ -> "detach daemon from console."

    let parser = ArgumentParser.Create<CLIArguments>(programName = "gadget.exe")
    let usage = parser.PrintUsage()
    let results = parser.Parse [| "--detach" ; "--listener" ; "localhost" ; "8080" |]
    let all = results.GetAllResults()
    let detach = results.Contains Detach
    let listener = results.GetResults Listener
    let dataOpt = results.TryGetResult Data
    let logLevel = results.GetResult (Log_Level, defaultValue = 0)
    let arguments = parser.PrintCommandLineArgumentsFlat [ Port 42 ; Working_Directory "temp" ]
    let xml = parser.PrintAppSettingsArguments [ Port 42 ; Working_Directory "/tmp" ]


    [<CliPrefix(CliPrefix.Dash)>]
    type SolverRunnerArguments =
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-t")>]  EndTime of float
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-y0")>] TotalAmount of float
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-a")>]  UseAbundant of bool
        | [<Unique>] [<EqualsAssignment>] [<AltCommandLine("-p")>]  PlotResults of bool

    with
        static member defaultValues =
            [
                EndTime 10_000.0
                TotalAmount 10.0
                UseAbundant false
                PlotResults true
            ]

        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | EndTime _ -> "specify tEnd."
                | TotalAmount _ -> "specify t0."
                | UseAbundant _ -> "specify if abundant substance is used."
                | PlotResults _ -> "specify if output charts to web browser."
