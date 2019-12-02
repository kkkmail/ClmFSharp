namespace ClmSys

open System
open System.Threading
open System.Configuration.Install
open System.ServiceProcess
open ClmSys.GeneralData
open Newtonsoft.Json
open ClmSys.GeneralErrors

module Wcf =

    let readFile (path : string) : Result<string, ReadFileError> = path |> FileNotFound |> Error
    let parseJson (content : string) : Result<string, JsonParseError> = content |> InvalidStructure |> Result.Error


    /// See https://stackoverflow.com/questions/53536450/merging-discriminated-unions-in-f
    let readJson (path : string) : Result<string, ReadJsonError> =
        readFile path
        |> Result.mapError ReadFileError
        |> Result.bind (fun content -> parseJson content |> Result.mapError JsonParseError)

