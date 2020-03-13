﻿namespace ClmDefaults
open Clm.ModelParams

module AllDefaults =

    let defaultValues =

        AllDefaults_000.defaultValues
        @
        AllDefaults_001.defaultValues
        @
        AllDefaults_002.defaultValues

        |> List.map (fun e -> e.clmDefaultValueId, e)
        |> Map.ofList


    let tryGetDefaultValues i = defaultValues.TryFind i
