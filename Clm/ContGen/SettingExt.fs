namespace ContGen

open Clm.Distributions
open Clm.ReactionRates
open DatabaseTypes

module SettingExt = 

    let tryFindByName (m : SystemSettingMap) n = m.TryFind n |> Option.bind (fun v -> v.TryFind 0L)


    let getFullName c n = c + SystemSetting.separator + n


    let getNameWithParent po c n = 
        let parent = 
            match po with 
            | Some p -> p + SystemSetting.separator
            | None -> EmptyString

        parent + (getFullName c n)


    let getClassNameWithParent po c = 
        let parent = 
            match po with 
            | Some p -> p + SystemSetting.separator
            | None -> EmptyString

        parent + c


    let getDoubleOpt (m : SystemSettingMap) po c n = 
        getNameWithParent po c n 
        |> tryFindByName m
        |> Option.bind (fun v -> v.settingFloat |> Some)


    type DistributionParams
        with

        static member className = "DistributionParams"

        static member thresholdName = "threshold"
        static member scaleName = "scale"
        static member shiftName = "shift"

        static member getValue (m : SystemSettingMap) po = 
            {
                threshold = getDoubleOpt m po DistributionParams.className DistributionParams.thresholdName
                scale = getDoubleOpt m po DistributionParams.className DistributionParams.scaleName
                shift = getDoubleOpt m po DistributionParams.className DistributionParams.shiftName
            }


    type Distribution
        with
        static member className = "Distribution"

        static member tryGet (m : SystemSettingMap) po =
            match getClassNameWithParent po Distribution.className |> tryFindByName m with 
            | Some v -> 
                match v.settingText with 
                | Some s -> 
                    match s with
                    | "Delta" -> (fun s p -> DeltaDistribution (s, p) |> Delta) |> Some
                    | "BiDelta" -> (fun s p -> BiDeltaDistribution (s, p) |> BiDelta) |> Some
                    | "Uniform" -> (fun s p -> UniformDistribution (s, p) |> Uniform) |> Some
                    | "Triangular" -> (fun s p -> TriangularDistribution (s, p) |> Triangular) |> Some
                    | "SymmetricTriangular" -> (fun s p -> SymmetricTriangularDistribution (s, p) |> SymmetricTriangular) |> Some
                    | _ -> None
                | None -> None
            | None -> None

    //type RateMultiplierDistribution = 
    //    | NoneRateMult
    //    | RateMultDistr of Distribution


    //type CatRatesEeParam = 
    //    {
    //        rateMultiplierDistr : RateMultiplierDistribution
    //        eeForwardDistribution : EeDistribution option
    //        eeBackwardDistribution : EeDistribution option
    //    }

    type CatRatesEeParam
        with 
        static member className = "CatRatesEeParam"

        static member tryGet (m : SystemSettingMap) po = failwith ""
            //match getClassNameWithParent po Distribution.className |> tryFindByName m with 
            //| Some v -> 
            //    match v.settingText with 
            //    | Some s -> 
            //        match s with
            //        | "Delta" -> (fun s p -> DeltaDistribution (s, p) |> Delta) |> Some
            //        | "BiDelta" -> (fun s p -> BiDeltaDistribution (s, p) |> BiDelta) |> Some
            //        | "Uniform" -> (fun s p -> UniformDistribution (s, p) |> Uniform) |> Some
            //        | "Triangular" -> (fun s p -> TriangularDistribution (s, p) |> Triangular) |> Some
            //        | "SymmetricTriangular" -> (fun s p -> SymmetricTriangularDistribution (s, p) |> SymmetricTriangular) |> Some
            //        | _ -> None
            //    | None -> None
            //| None -> None


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

        static member tryGet (m : SystemSettingMap) seed = 
            let po = (Some SynthesisRandomParam.className)
            let getDoubleOpt = getDoubleOpt m po SynthesisRandomParam.className

            match Distribution.tryGet m po with
            | Some d ->
                {
                    synthesisDistribution = d seed (DistributionParams.getValue m po)
                    forwardScale = getDoubleOpt SynthesisRandomParam.forwardScaleName
                    backwardScale = getDoubleOpt SynthesisRandomParam.backwardScaleName
                }
                |> Some
            | None -> None


    //type CatalyticSynthesisRandomParam = 
    //    {
    //        catSynthRndEeParams : CatRatesEeParam
    //    }


