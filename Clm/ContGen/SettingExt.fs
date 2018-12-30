namespace ContGen

open Clm.Distributions
open Clm.ReactionRates
open DatabaseTypes

module SettingExt = 

    let tryFindByName (m : SystemSettingMap) n = m.TryFind n |> Option.bind (fun v -> v.TryFind 0L)
    let addParent p np = p @ [ np ]

    let getDoubleOpt (m : SystemSettingMap) po n = 
        addParent po n
        |> tryFindByName m
        |> Option.bind (fun v -> v.settingFloat |> Some)


    let getTextOpt (m : SystemSettingMap) po n = 
        addParent po n
        |> tryFindByName m
        |> Option.bind (fun v -> v.settingText)


    type DistributionParams
        with
        static member thresholdName = "threshold"
        static member scaleName = "scale"
        static member shiftName = "shift"

        static member getValue (m : SystemSettingMap) po = 
            {
                threshold = getDoubleOpt m po DistributionParams.thresholdName
                scale = getDoubleOpt m po DistributionParams.scaleName
                shift = getDoubleOpt m po DistributionParams.shiftName
            }


    type Distribution
        with
        static member className = "Distribution"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po =
            match getTextOpt m po Distribution.className with
            | Some s -> 
                let seed = seeder()
                let p = DistributionParams.getValue m po

                match s with
                | "Delta" -> DeltaDistribution (seed, p) |> Delta |> Some
                | "BiDelta" -> BiDeltaDistribution (seed, p) |> BiDelta |> Some
                | "Uniform" -> UniformDistribution (seed, p) |> Uniform |> Some
                | "Triangular" -> TriangularDistribution (seed, p) |> Triangular |> Some
                | "SymmetricTriangular" -> SymmetricTriangularDistribution (seed, p) |> SymmetricTriangular |> Some
                | _ -> None
            | None -> None


    type RateMultiplierDistribution
        with
        static member className = "RateMultiplierDistribution"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po =
            match getTextOpt m po RateMultiplierDistribution.className with
            | Some s ->
                match s with
                | "NoneRateMult" -> NoneRateMult |> Some
                | "RateMultDistr" -> Distribution.tryGet m seeder po |> Option.bind (fun d -> RateMultDistr d |> Some)
                | _ -> None
            | None -> None


    type EeDistribution with
        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po =
            Distribution.tryGet m seeder po |> Option.bind (fun d -> EeDistribution d |> Some)


    type CatRatesEeParam
        with
        static member rateMultiplierDistrName = "rateMultiplierDistr"
        static member eeForwardDistributionName = "eeForwardDistribution"
        static member eeBackwardDistributionName = "eeBackwardDistribution"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po =
            match addParent po CatRatesEeParam.rateMultiplierDistrName |> RateMultiplierDistribution.tryGet m seeder with
            | Some r ->
                {
                    rateMultiplierDistr = r
                    eeForwardDistribution = addParent po CatRatesEeParam.eeForwardDistributionName |> EeDistribution.tryGet m seeder
                    eeBackwardDistribution= addParent po CatRatesEeParam.eeBackwardDistributionName |> EeDistribution.tryGet m seeder
                }
                |> Some
            | None -> None


    type FoodCreationParam
        with

        static member className = "FoodCreationParam"
        static member foodCreationRateName = "foodCreationRate"

        static member tryGet (m : SystemSettingMap) po = 
            match getDoubleOpt m po FoodCreationParam.foodCreationRateName with
            | Some v ->
                {
                    foodCreationRate = v
                }
                |> Some
            | None -> None


    type WasteRemovalParam
        with

        static member className = "WasteRemovalParam"
        static member wasteRemovalRateName = "wasteRemovalRate"

        static member tryGet (m : SystemSettingMap) po = 
            match getDoubleOpt m po WasteRemovalParam.wasteRemovalRateName with
            | Some v -> 
                {
                    wasteRemovalRate = v
                }
                |> Some
            | None -> None


    type WasteRecyclingParam
        with

        static member className = "WasteRecyclingParam"
        static member wasteRecyclingRateName = "wasteRecyclingRate"

        static member tryGet (m : SystemSettingMap) po = 
            match getDoubleOpt m po WasteRecyclingParam.wasteRecyclingRateName with
            | Some v -> 
                {
                    wasteRecyclingRate = v
                }
                |> Some
            | None -> None


    type SynthesisRandomParam
        with
        static member synthesisDistributionName = "synthesisDistribution"
        static member forwardScaleName = "forwardScale"
        static member backwardScaleName = "backwardScale"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po = 
            match addParent po SynthesisRandomParam.synthesisDistributionName |> Distribution.tryGet m seeder with
            | Some d ->
                {
                    synthesisDistribution = d
                    forwardScale = getDoubleOpt m po SynthesisRandomParam.forwardScaleName
                    backwardScale = getDoubleOpt m po SynthesisRandomParam.backwardScaleName
                }
                |> Some
            | None -> None


    type CatalyticSynthesisRandomParam
        with
        static member catSynthRndEeParamsName = "catSynthRndEeParams"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po = 
            match addParent po CatalyticSynthesisRandomParam.catSynthRndEeParamsName |> CatRatesEeParam.tryGet m seeder with
            | Some d ->
                {
                    catSynthRndEeParams = d
                }
                |> Some
            | None -> None


    //type CatRatesSimilarityParam =
    //    {
    //        simBaseDistribution : Distribution
    //        getRateMultiplierDistr : RateMultiplierDistributionGetter
    //        getForwardEeDistr : EeDistributionGetter
    //        getBackwardEeDistr : EeDistributionGetter
    //    }
