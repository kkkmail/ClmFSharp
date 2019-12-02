namespace ClmSys

open System
open System.Threading
open System.Configuration.Install
open System.ServiceProcess
open ClmSys.GeneralData
open Newtonsoft.Json
open ClmSys.GeneralErrors

/// See https://stackoverflow.com/questions/53536450/merging-discriminated-unions-in-f
module Wcf =

    let readFile (path : string) : Result<string, ReadFileError> =
        let x = Result.mapError
        path |> FileNotFound |> Error


    let parseJson (content : string) : Result<string, JsonParseError> = content |> InvalidStructure |> Result.Error


    //let readJson (path : string) : Result<string, ReadJsonError> =
    //    match path |> readFile with
    //    | Ok content ->
    //        match content |> parseJson with
    //        | Ok json -> Ok json
    //        | Error e -> Error (ReadJsonError.JsonParseError e)
    //    | Error e -> Error (ReadJsonError.ReadFileError e)


    let readJson (path : string) : Result<string, ReadJsonError> =
        readFile path
        |> Result.mapError ReadFileError
        |> Result.bind (fun content -> parseJson content |> Result.mapError JsonParseError)


    ///
    let tryTransmit<'A, 'B> (a : 'A) : Result<'B, exn> =
        failwith ""

