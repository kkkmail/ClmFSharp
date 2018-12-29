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


    let getFloatOpt (m : SystemSettingMap) po c n = 
        getNameWithParent po c n 
        |> tryFindByName m
        |> Option.bind (fun v -> v.settingFloat |> Some)


    type DistributionParams
        with

        static member className = "DistributionParams"
        static member thresholdName = "threshold"
        static member scaleName = "scale"
        static member shiftName = "shift"

        static member tryGet (m : SystemSettingMap) po = 
            {
                threshold = getFloatOpt m po DistributionParams.className DistributionParams.thresholdName
                scale = getFloatOpt m po DistributionParams.className DistributionParams.scaleName
                shift = getFloatOpt m po DistributionParams.className DistributionParams.shiftName
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
        static member wasteRemovalRateName = "WasteRemovalParam.wasteRemovalRate"

        static member tryGet (m : SystemSettingMap) = 
            match tryFindByName m WasteRemovalParam.wasteRemovalRateName with 
            | Some v -> 
                {
                    wasteRemovalRate = v.settingFloat
                }
                |> Some
            | None -> None


    type WasteRecyclingParam
        with

        static member wasteRecyclingRateName = "WasteRecyclingParam.wasteRecyclingRate"

        static member tryGet (m : SystemSettingMap) = 
            match tryFindByName m WasteRecyclingParam.wasteRecyclingRateName with 
            | Some v -> 
                {
                    wasteRecyclingRate = v.settingFloat
                }
                |> Some
            | None -> None


    //type SynthesisRandomParam = 
    //    {
    //        synthesisDistribution : Distribution
    //        forwardScale : double option
    //        backwardScale : double option
    //    }


    type SynthesisRandomParam
        with

        static member className = "SynthesisRandomParam"
        static member synthesisDistributionName = "synthesisDistribution"


        static member tryGet (m : SystemSettingMap) = 
            match SynthesisRandomParam.synthesisDistributionName |> SynthesisRandomParam.getFullName |> tryFindByName m with 
            //match tryFindByName m WasteRecyclingParam.wasteRecyclingRateName with 
            | Some v -> 
                {
                    synthesisDistribution = failwith ""
                    forwardScale = None
                    backwardScale = None
                }
                |> Some
            | None -> None
