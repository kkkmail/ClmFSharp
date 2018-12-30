namespace ContGen

open Clm.Distributions
open Clm.ReactionRates
open DatabaseTypes

module SettingExt = 

    let tryFindByName (m : SystemSettingMap) n = m.TryFind n |> Option.bind (fun v -> v.TryFind 0L)
    let tryFindByNameOpt (m : SystemSettingMap) no = no |> Option.bind (fun n -> tryFindByName m n)


    let getFullName c n = c + SystemSetting.separator + n


    let addParent (po : string option) (np : string) = 
        match po with
        | Some p -> p + np |> Some
        | None -> Some np


    //let getNameWithParent po c n = 
    //    let parent = 
    //        match po with 
    //        | Some p -> p + SystemSetting.separator
    //        | None -> EmptyString

    //    parent + (getFullName c n)


    //let getClassNameWithParent po c = 
    //    let parent = 
    //        match po with 
    //        | Some p -> p + SystemSetting.separator
    //        | None -> EmptyString

    //    parent + c


    let getDoubleOpt (m : SystemSettingMap) po n = 
        addParent po n
        |> tryFindByNameOpt m
        |> Option.bind (fun v -> v.settingFloat |> Some)


    let getTextOpt (m : SystemSettingMap) po n = 
        addParent po n
        |> tryFindByNameOpt m
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

        static member tryGet (m : SystemSettingMap) = 
            match getFullName FoodCreationParam.className FoodCreationParam.foodCreationRateName |> tryFindByName m with
            | Some v ->
                {
                    foodCreationRate = v.settingFloat
                }
                |> Some
            | None -> None


    type WasteRemovalParam
        with

        static member className = "WasteRemovalParam"
        static member wasteRemovalRateName = "wasteRemovalRate"

        static member tryGet (m : SystemSettingMap) = 
            match getFullName WasteRemovalParam.className WasteRemovalParam.wasteRemovalRateName |> tryFindByName m with
            | Some v -> 
                {
                    wasteRemovalRate = v.settingFloat
                }
                |> Some
            | None -> None


    type WasteRecyclingParam
        with

        static member className = "WasteRecyclingParam"
        static member wasteRecyclingRateName = "wasteRecyclingRate"

        static member tryGet (m : SystemSettingMap) = 
            match getFullName WasteRecyclingParam.className WasteRecyclingParam.wasteRecyclingRateName |> tryFindByName m with
            | Some v -> 
                {
                    wasteRecyclingRate = v.settingFloat
                }
                |> Some
            | None -> None


    type SynthesisRandomParam
        with

        static member className = "SynthesisRandomParam"

        static member synthesisDistributionName = "synthesisDistribution"
        static member forwardScaleName = "forwardScale"
        static member backwardScaleName = "backwardScale"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) = 
            let po = (Some SynthesisRandomParam.className)
            let pdo = addParent po SynthesisRandomParam.synthesisDistributionName
            let getDoubleOpt = getDoubleOpt m po SynthesisRandomParam.className

            match Distribution.tryGet m pdo with
            | Some d ->
                {
                    synthesisDistribution = d (seeder()) (DistributionParams.getValue m pdo)
                    forwardScale = getDoubleOpt SynthesisRandomParam.forwardScaleName
                    backwardScale = getDoubleOpt SynthesisRandomParam.backwardScaleName
                }
                |> Some
            | None -> None


    //type CatalyticSynthesisRandomParam = 
    //    {
    //        catSynthRndEeParams : CatRatesEeParam
    //    }


