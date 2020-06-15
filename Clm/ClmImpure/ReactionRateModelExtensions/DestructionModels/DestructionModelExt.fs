﻿namespace ClmImpure.ReactionRateModelExtensions

open Clm.ReactionRates
open Clm.ReactionRatesExt
open ClmImpure.ReactionRateModels
open ClmImpure.ReactionRateModels.DestructionRandomModel
open ClmImpure.ReactionRateModels.DestructionModel
open ClmImpure.ReactionRateModelExtensions.ReactionRateModelExtBase

module DestructionModelExt =

    type DestructionModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | DestructionRateModel d -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            tryCreateModel DestructionRandomParam.paramGetter (fun d -> d |> DestructionRandomModel |> DestrRndModel |> DestructionRateModel) (p, m)
