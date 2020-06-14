namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRates
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelsBase

module FoodCreationModel =

    type FoodCreationModel (p : FoodCreationParam) =
        inherit RateModel<FoodCreationParam, FoodCreationReaction>(p)
        let calculateRates _ = getRates (Some p.foodCreationRate, Some 1.0) (None, None)
        member model.getRates r = getRatesImpl model.rateDictionary getEnantiomer calculateRates r
