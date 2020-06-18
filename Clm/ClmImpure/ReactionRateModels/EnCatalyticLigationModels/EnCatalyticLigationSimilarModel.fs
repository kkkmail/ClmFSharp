﻿namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRatesBase
open Clm.ReactionRateParams
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.EnCatalyticLigationRandomModel

module EnCatalyticLigationSimilarModel =

    type EnCatalyticLigationSimilarParamWithModel =
        {
            enCatLigModel : EnCatalyticLigationRandomModel
            peptideBondData : PeptideBondData
            enCatLigSimParam : CatRatesSimilarityParam
        }


    type EnCatalyticLigationSimilarModel (p : EnCatalyticLigationSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (EnCatalyticLigationReaction (s, c, u)) =
            let (LigationReaction a) = s
            {
                reaction = s
                catalyst = c
                energySource = u
                getReactionData = fun r -> p.peptideBondData.findSameBondSymmetry r.peptideBond
                getMatchingReactionMult = fun x -> x
                inverse = fun r -> r.peptideBond
                getCatEnantiomer = getEnantiomer
                getEnergySourceEnantiomer = getEnantiomer
                enCatReactionCreator = EnCatalyticLigationReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = fun e -> p.peptideBondData.findSameBond e
                getBaseRates = p.enCatLigModel.inputParams.ligationModel.getRates rnd
                getBaseCatRates = p.enCatLigModel.getRates rnd t
                simParams = p.enCatLigSimParam
                eeParams = p.enCatLigModel.inputParams.enCatLigationParam.enCatLigRndEeParams
                rateDictionary = p.enCatLigModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateEnSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.enCatLigModel.rateDictionary
