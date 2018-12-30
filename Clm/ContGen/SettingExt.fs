namespace ContGen

open Clm.Distributions
open Clm.ReactionRates
open DatabaseTypes

module SettingExt = 

    let tryFindByName (m : SystemSettingMap) n = m.TryFind n
    let addParent p np = p @ [ (np, 0) ]

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
                let p = DistributionParams.getValue m po

                match s with
                | "Delta" -> DeltaDistribution (seeder(), p) |> Delta |> Some
                | "BiDelta" -> BiDeltaDistribution (seeder(), p) |> BiDelta |> Some
                | "Uniform" -> UniformDistribution (seeder(), p) |> Uniform |> Some
                | "Triangular" -> TriangularDistribution (seeder(), p) |> Triangular |> Some
                | "SymmetricTriangular" -> SymmetricTriangularDistribution (seeder(), p) |> SymmetricTriangular |> Some
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


    type RateMultiplierDistributionGetter
        with
        static member className = "RateMultiplierDistributionGetter"

        static member tryGet (m : SystemSettingMap) po =
            match getTextOpt m po RateMultiplierDistributionGetter.className with
            | Some s -> 
                match s with
                | "NoneRateMultDistrGetter" -> NoneRateMultDistrGetter |> Some
                | "DeltaRateMultDistrGetter" -> DeltaRateMultDistrGetter |> Some
                | "TriangularRateMultDistrGetter" -> TriangularRateMultDistrGetter |> Some
                | "SymmetricTriangularRateMultDistrGetter" -> SymmetricTriangularRateMultDistrGetter |> Some
                | _ -> None
            | None -> None


    type EeDistributionGetter
        with
        static member className = "EeDistributionGetter"

        static member tryGet (m : SystemSettingMap) po =
            match getTextOpt m po EeDistributionGetter.className with
            | Some s -> 
                match s with
                | "NoneEeGetter" -> NoneEeGetter |> Some
                | "DeltaEeDistributionGetter" -> DeltaEeDistributionGetter |> Some
                | "CenteredEeDistributionGetter" -> CenteredEeDistributionGetter |> Some
                | _ -> None
            | None -> None


    type CatRatesSimilarityParam
        with
        static member simBaseDistributionName = "simBaseDistribution"
        static member getRateMultiplierDistrName = "getRateMultiplierDistr"
        static member getForwardEeDistrName = "getForwardEeDistr"
        static member getBackwardEeDistrName = "getBackwardEeDistr"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po = 
            let d() = addParent po CatRatesSimilarityParam.simBaseDistributionName |> Distribution.tryGet m seeder
            let r() = addParent po CatRatesSimilarityParam.getRateMultiplierDistrName |> RateMultiplierDistributionGetter.tryGet m
            let f() = addParent po CatRatesSimilarityParam.getForwardEeDistrName |> EeDistributionGetter.tryGet m
            let b() = addParent po CatRatesSimilarityParam.getBackwardEeDistrName |> EeDistributionGetter.tryGet m

            match d(), r(), f(), b() with
            | Some d1, Some r1, Some f1, Some b1 ->
                {
                    simBaseDistribution = d1
                    getRateMultiplierDistr = r1
                    getForwardEeDistr = f1
                    getBackwardEeDistr = b1
                }
                |> Some
            | _ -> None


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


    type SynthesisParam
        with
        static member className = "SynthesisParam"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po =
            match getTextOpt m po SynthesisParam.className with
            | Some s -> 
                match s with
                | "SynthRndParam" -> 
                    addParent po "SynthRndParam" 
                    |> SynthesisRandomParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> SynthRndParam |> Some)
                | _ -> None
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


    type CatalyticSynthesisParam
        with
        static member className = "CatalyticSynthesisParam"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po =
            match getTextOpt m po CatalyticSynthesisParam.className with
            | Some s -> 
                match s with
                | "CatSynthRndParam" -> 
                    addParent po "CatSynthRndParam" 
                    |> CatalyticSynthesisRandomParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> CatSynthRndParam |> Some)
                | "CatSynthSimParam" -> 
                    addParent po "CatSynthSimParam" 
                    |> CatRatesSimilarityParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> CatSynthSimParam |> Some)
                | _ -> None
            | None -> None


    type DestructionRandomParam
        with
        static member destructionDistributionName = "destructionDistribution"
        static member forwardScaleName = "forwardScale"
        static member backwardScaleName = "backwardScale"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po = 
            match addParent po DestructionRandomParam.destructionDistributionName |> Distribution.tryGet m seeder with
            | Some d ->
                {
                    destructionDistribution = d
                    forwardScale = getDoubleOpt m po DestructionRandomParam.forwardScaleName
                    backwardScale = getDoubleOpt m po DestructionRandomParam.backwardScaleName
                }
                |> Some
            | None -> None


    type DestructionParam
        with
        static member className = "DestructionParam"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po =
            match getTextOpt m po DestructionParam.className with
            | Some s -> 
                match s with
                | "DestrRndParam" -> 
                    addParent po "DestrRndParam"
                    |> DestructionRandomParam.tryGet m seeder
                    |> Option.bind (fun e -> e |> DestrRndParam |> Some)
                | _ -> None
            | None -> None


    type CatalyticDestructionRandomParam
        with
        static member catDestrRndEeParamsName = "catDestrRndEeParams"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po = 
            match addParent po CatalyticDestructionRandomParam.catDestrRndEeParamsName |> CatRatesEeParam.tryGet m seeder with
            | Some d ->
                {
                    catDestrRndEeParams = d
                }
                |> Some
            | None -> None


    type CatalyticDestructionParam
        with
        static member className = "CatalyticDestructionParam"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po =
            match getTextOpt m po CatalyticDestructionParam.className with
            | Some s -> 
                match s with
                | "CatDestrRndParam" -> 
                    addParent po "CatDestrRndParam" 
                    |> CatalyticDestructionRandomParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> CatDestrRndParam |> Some)
                | "CatDestrSimParam" -> 
                    addParent po "CatDestrSimParam" 
                    |> CatRatesSimilarityParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> CatDestrSimParam |> Some)
                | _ -> None
            | None -> None


    type SedimentationDirectRandomParam
        with
        static member sedimentationDirectDistributionName = "sedimentationDirectDistribution"
        static member forwardScaleName = "forwardScale"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po = 
            match addParent po SedimentationDirectRandomParam.sedimentationDirectDistributionName |> Distribution.tryGet m seeder with
            | Some d ->
                {
                    sedimentationDirectDistribution = d
                    forwardScale = getDoubleOpt m po SedimentationDirectRandomParam.forwardScaleName
                }
                |> Some
            | None -> None


    type SedimentationDirectParam
        with
        static member className = "SedimentationDirectParam"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po =
            match getTextOpt m po SedimentationDirectParam.className with
            | Some s -> 
                match s with
                | "SedDirRndParam" -> 
                    addParent po "SedDirRndParam"
                    |> SedimentationDirectRandomParam.tryGet m seeder
                    |> Option.bind (fun e -> e |> SedDirRndParam |> Some)
                | _ -> None
            | None -> None


    type SedimentationAllRandomParam
        with
        static member sedimentationAllDistributionName = "sedimentationAllDistribution"
        static member forwardScaleName = "forwardScale"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po = 
            match addParent po SedimentationAllRandomParam.sedimentationAllDistributionName |> Distribution.tryGet m seeder with
            | Some d ->
                {
                    sedimentationAllDistribution = d
                    forwardScale = getDoubleOpt m po SedimentationAllRandomParam.forwardScaleName
                }
                |> Some
            | None -> None


    type SedimentationAllParam
        with
        static member className = "SedimentationAllParam"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po =
            match getTextOpt m po SedimentationAllParam.className with
            | Some s -> 
                match s with
                | "SedAllRndParam" -> 
                    addParent po "SedAllRndParam"
                    |> SedimentationAllRandomParam.tryGet m seeder
                    |> Option.bind (fun e -> e |> SedAllRndParam |> Some)
                | _ -> None
            | None -> None


    type LigationRandomParam
        with
        static member ligationDistributionName = "ligationDistribution"
        static member forwardScaleName = "forwardScale"
        static member backwardScaleName = "backwardScale"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po = 
            match addParent po LigationRandomParam.ligationDistributionName |> Distribution.tryGet m seeder with
            | Some d ->
                {
                    ligationDistribution = d
                    forwardScale = getDoubleOpt m po LigationRandomParam.forwardScaleName
                    backwardScale = getDoubleOpt m po LigationRandomParam.backwardScaleName
                }
                |> Some
            | None -> None


    type LigationParam
        with
        static member className = "LigationParam"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po =
            match getTextOpt m po LigationParam.className with
            | Some s -> 
                match s with
                | "LigRndParam" -> 
                    addParent po "LigRndParam"
                    |> LigationRandomParam.tryGet m seeder
                    |> Option.bind (fun e -> e |> LigRndParam |> Some)
                | _ -> None
            | None -> None


    type CatalyticLigationRandomParam
        with
        static member catLigRndEeParamsName = "catLigRndEeParams"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po = 
            match addParent po CatalyticLigationRandomParam.catLigRndEeParamsName |> CatRatesEeParam.tryGet m seeder with
            | Some d ->
                {
                    catLigRndEeParams = d
                }
                |> Some
            | None -> None


    type CatalyticLigationParam
        with
        static member className = "CatalyticLigationParam"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po =
            match getTextOpt m po CatalyticLigationParam.className with
            | Some s -> 
                match s with
                | "CatLigRndParam" -> 
                    addParent po "CatLigRndParam" 
                    |> CatalyticLigationRandomParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> CatLigRndParam |> Some)
                | _ -> None
            | None -> None


    type RacemizationRandomParam
        with
        static member racemizationDistributionName = "racemizationDistribution"
        static member forwardScaleName = "forwardScale"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po = 
            match addParent po RacemizationRandomParam.racemizationDistributionName |> Distribution.tryGet m seeder with
            | Some d ->
                {
                    racemizationDistribution = d
                    forwardScale = getDoubleOpt m po RacemizationRandomParam.forwardScaleName
                }
                |> Some
            | None -> None


    type RacemizationParam
        with
        static member className = "RacemizationParam"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po =
            match getTextOpt m po RacemizationParam.className with
            | Some s -> 
                match s with
                | "RacemRndParam" -> 
                    addParent po "RacemRndParam"
                    |> RacemizationRandomParam.tryGet m seeder
                    |> Option.bind (fun e -> e |> RacemRndParam |> Some)
                | _ -> None
            | None -> None


    type CatalyticRacemizationRandomParam
        with
        static member catRacemRndEeParamsName = "catRacemRndEeParams"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po = 
            match addParent po CatalyticRacemizationRandomParam.catRacemRndEeParamsName |> CatRatesEeParam.tryGet m seeder with
            | Some d ->
                {
                    catRacemRndEeParams = d
                }
                |> Some
            | None -> None


    type CatalyticRacemizationParam
        with
        static member className = "CatalyticRacemizationParam"

        static member tryGet (m : SystemSettingMap) (seeder : unit -> int) po =
            match getTextOpt m po CatalyticRacemizationParam.className with
            | Some s -> 
                match s with
                | "CatRacemRndParam" -> 
                    addParent po "CatRacemRndParam" 
                    |> CatalyticRacemizationRandomParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> CatRacemRndParam |> Some)
                | "CatRacemSimParam" -> 
                    addParent po "CatRacemSimParam" 
                    |> CatRatesSimilarityParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> CatRacemSimParam |> Some)
                | _ -> None
            | None -> None


    //type ReactionRateModelParam = 
    //    | FoodCreationRateParam of FoodCreationParam
    //    | WasteRemovalRateParam of WasteRemovalParam
    //    | WasteRecyclingRateParam of WasteRecyclingParam
    //    | SynthesisRateParam of SynthesisParam
    //    | DestructionRateParam of DestructionParam
    //    | CatalyticSynthesisRateParam of CatalyticSynthesisParam
    //    | CatalyticDestructionRateParam of CatalyticDestructionParam
    //    | LigationRateParam of LigationParam
    //    | CatalyticLigationRateParam of CatalyticLigationParam
    //    | SedimentationDirectRateParam of SedimentationDirectParam
    //    | SedimentationAllRateParam of SedimentationAllParam
    //    | RacemizationRateParam of RacemizationParam
    //    | CatalyticRacemizationRateParam of CatalyticRacemizationParam
