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


    //type DestructionRandomParam = 
    //    {
    //        destructionDistribution : Distribution
    //        forwardScale : double option
    //        backwardScale : double option
    //    }


    //type DestructionParam = 
    //    | DestrRndParam of DestructionRandomParam


    //type CatalyticDestructionRandomParam = 
    //    {
    //        catDestrRndEeParams : CatRatesEeParam
    //    }


    //type CatalyticDestructionParam = 
    //    | CatDestrRndParam of CatalyticDestructionRandomParam
    //    | CatDestrSimParam of CatRatesSimilarityParam

    //type SedimentationDirectRandomParam = 
    //    {
    //        sedimentationDirectDistribution : Distribution
    //        forwardScale : double option
    //    }


    //type SedimentationDirectParam = 
    //    | SedDirRndParam of SedimentationDirectRandomParam

    //type SedimentationAllRandomParam = 
    //    {
    //        sedimentationAllDistribution : Distribution
    //        forwardScale : double option
    //    }


    //type SedimentationAllParam = 
    //    | SedAllRndParam of SedimentationAllRandomParam


    //type LigationRandomParam = 
    //    {
    //        ligationDistribution : Distribution
    //        forwardScale : double option
    //        backwardScale : double option
    //    }


    //type LigationParam = 
    //    | LigRndParam of LigationRandomParam

    //type CatalyticLigationRandomParam = 
    //    {
    //        catLigRndEeParams : CatRatesEeParam
    //    }


    //type CatalyticLigationParam = 
    //    | CatLigRndParam of CatalyticLigationRandomParam

    //type RacemizationRandomParam = 
    //    {
    //        racemizationDistribution : Distribution
    //        forwardScale : double option
    //    }


    //type RacemizationParam = 
    //    | RacemRndParam of RacemizationRandomParam

    //type CatalyticRacemizationRandomParam = 
    //    {
    //        catRacemRndEeParams : CatRatesEeParam
    //    }


    //type CatalyticRacemizationSimilarParam =
    //    {
    //        simRacemDistribution : Distribution
    //        aminoAcids : list<AminoAcid>
    //    }


    //type CatalyticRacemizationParam = 
    //    | CatRacemRndParam of CatalyticRacemizationRandomParam
    //    | CatRacemSimParam of CatRatesSimilarityParam


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


