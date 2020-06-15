﻿namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRates
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase

module LigationRandomModel =

    type LigationRandomModel (p : LigationRandomParam) =
        inherit RateModel<LigationRandomParam, LigationReaction>(p)

        let calculateRates rnd _ =
            let d = p.ligationDistribution
            getRates (p.forwardScale, d.nextDouble rnd |> Some) (p.backwardScale, d.nextDouble rnd |> Some)

        member model.getRates rnd r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd) r