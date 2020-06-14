namespace ClmImpure.ReactionRateModels

open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRates
open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels.ReactionRateModelBase

module WasteRemovalModel =

    type WasteRemovalModel (p : WasteRemovalParam) =
        inherit RateModel<WasteRemovalParam, WasteRemovalReaction>(p)
        let calculateRates _ = getRates (Some p.wasteRemovalRate, Some 1.0) (None, None)
        member model.getRates r = getRatesImpl model.rateDictionary getEnantiomer calculateRates r
