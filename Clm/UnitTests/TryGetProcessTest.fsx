#r "System.Core.dll"
#r "System.dll"
#r "System.Numerics.dll"
#r @"..\packages\log4net.2.0.8\lib\net45-full\log4net.dll"
#r @"..\packages\FsPickler.5.2.2\lib\net45\FsPickler.dll"
#r @"..\packages\Newtonsoft.Json.12.0.3\lib\net45\Newtonsoft.Json.dll"
#r @"..\packages\Argu.6.0.0\lib\netstandard2.0\Argu.dll"

#load @"..\ClmSys\VersionInfo.fs"
#load @"..\ClmSys\Logging.fs"
#load @"..\ClmSys\GeneralErrors.fs"
#load @"..\ClmSys\GeneralData.fs"
#load @"..\Clm\CommandLine.fs"

open ClmSys.GeneralData
open Clm.CommandLine

let v = LocalProcessId 16132

match tryGetProcessById v with
| Some p ->
    printfn "Found process with id: %A" v

    match tryGetProcessName p with
    | Some n when n = SolverRunnerProcessName ->
        printfn "Found process with name: %s" n
    | m ->
        printfn "CANNOT find process with name: '%s', but found '%A'" SolverRunnerName m
| None ->
    printfn "CANNOT find process with id: %A" v
