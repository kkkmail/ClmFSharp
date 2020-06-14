namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRates
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelsBase

module WasteRecyclingModel =

    type WasteRecyclingModel (p : WasteRecyclingParam) =
        inherit RateModel<WasteRecyclingParam, WasteRecyclingReaction>(p)
        let calculateRates _ = getRates (Some p.wasteRecyclingRate, Some 1.0) (None, None)
        member model.getRates r = getRatesImpl model.rateDictionary getEnantiomer calculateRates r
