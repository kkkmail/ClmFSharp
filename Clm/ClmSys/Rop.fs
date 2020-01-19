namespace ClmSys

/// https://fsharpforfunandprofit.com/posts/recipe-part2/
module Rop =

    /// convert a single value into a two-track result
    let succeed x = Ok x


    /// convert a single value into a two-track result
    let fail x = Error x


    /// apply either a success function or failure function
    let either successFunc failureFunc twoTrackInput =
        match twoTrackInput with
        | Ok s -> successFunc s
        | Error f -> failureFunc f


    /// convert a switch function into a two-track function
    let bind g = either g fail


    /// Converts a regular "success" function into a two-track function.
    let bindSuccess g r =
        match r with
        | Ok s -> Ok (g s)
        | Error e -> Error e


    let bindSuccessOption g w r =
        match r with
        | Ok (Some s) -> Ok (g s)
        | Ok None -> w
        | Error e -> Error e


    let bindErrOption g f r =
        match r with
        | Ok (Some s) -> Ok (g s)
        | Ok None -> Error f
        | Error e -> Error e


    /// pipe a two-track value into a switch function
    let (>>=) x f = bind f x


    /// compose two switches into another switch
    let (>=>) s1 s2 = s1 >> bind s2


    /// convert a one-track function into a switch
    let switch f = f >> succeed


    /// convert a one-track function into a two-track function
    let map g = either (g >> succeed) fail


    let mapFailure f = either succeed (f >> fail)

    /// convert a dead-end function into a one-track function
    let tee f x = f x; x


    /// convert a one-track function into a switch with exception handling
    let tryCatch g exnHandler x =
        try
            g x |> succeed
        with
        | ex -> exnHandler ex |> fail


    /// convert two one-track functions into a two-track function
    let doubleMap successFunc failureFunc =
        either (successFunc >> succeed) (failureFunc >> fail)


    /// add two switches in parallel
    let plus addSuccess addFailure switch1 switch2 x =
        match (switch1 x), (switch2 x) with
        | Ok s1, Ok s2 -> Ok (addSuccess s1 s2)
        | Error f1, Ok _ -> Error f1
        | Ok _, Error f2 -> Error f2
        | Error f1, Error f2 -> Error (addFailure f1 f2)


    /// Unwraps Result<List<Result<'A, 'B>>, 'C> into a list of successes only using given continuation functions.
    /// Use it for replacing failures with some default values.
    let unwrapAll a b r =
        match r with
        | Ok v -> v |> List.map (fun e -> match e with | Ok x -> Some x | Error f -> b f)
        | Error f -> a f
        |> List.choose id


    /// Same as above but extracts only successes.
    /// Use it for logging the failures.
    let unwrapSuccess a b r =
        match r with
        | Ok v ->
            v
            |> List.map (fun e ->
                    match e with
                    | Ok x -> Some x
                    | Error f ->
                        b f |> ignore
                        None)
        | Error f ->
            a f |> ignore
            []
        |> List.choose id


    //let unwrapFailure a b r =
    //    match r with
    //    | Ok v ->
    //        v
    //        |> List.map (fun e ->
    //                match e with
    //                | Ok x -> Some x
    //                | Error f ->
    //                    b f |> ignore
    //                    None)
    //    | Error f ->
    //        a f |> ignore
    //        []
    //    |> List.choose id


    /// Splits the list of results into list of successes and list of failures.
    let unzip r =
        let success e =
            match e with
            | Ok v -> Some v
            | Error _ -> None

        let failure e =
            match e with
            | Ok _ -> None
            | Error e -> Some e

        let sf e = success e, failure e
        let s, f = r |> List.map sf |> List.unzip
        s |> List.choose id, f |> List.choose id


    /// Updates the state using success or failure.
    let x successFunc failureFunc state twoTrackInput =
        match twoTrackInput with
        | Ok v -> successFunc state v
        | Error f -> failureFunc state f


    /// Updates the state if success and calls falure function (logger) in case of failure.
    let y successFunc failureFunc state twoTrackInput =
        match twoTrackInput with
        | Ok v -> successFunc state v
        | Error f ->
            failureFunc f
            state
