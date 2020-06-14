namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRates
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelsBase
open ClmImpure.ReactionRateModels.SynthesisModel

module CatalyticSynthesisRandomModel =

    type CatalyticSynthesisRandomParamWithModel =
        {
            catSynthRndParam : CatalyticSynthesisRandomParam
            synthesisModel : SynthesisModel
        }


    type CatalyticSynthesisRandomModel (p : CatalyticSynthesisRandomParamWithModel) =
        inherit RateModel<CatalyticSynthesisRandomParamWithModel, CatalyticSynthesisReaction>(p)

        let calculateCatSynthRates rnd t (CatalyticSynthesisReaction (s, c)) =
            {
                reaction = s
                catalyst = c
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticSynthesisReaction
                getBaseRates = p.synthesisModel.getRates rnd
                eeParams = p.catSynthRndParam.catSynthRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateCatSynthRates rnd t) r
