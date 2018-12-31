namespace ContGen

open Clm.Distributions
open Clm.ReactionRates
open DatabaseTypes

module SettingsExt = 

    let tryFindByName (m : SettingMap) n = m.TryFind n
    let addParent p n = p @ [ (n, 0) ]
    let add a b = a @ b


    let addOpt ao b =
        match ao with
        | Some a -> a @ b
        | None -> b


    let setDouble p n v = { Setting.defaultValue with settingPath = addParent p n; settingFloat = v }
    let setText p n v = { Setting.defaultValue with settingPath = addParent p n; settingText = Some v }
    let setDoubleOpt p n vo = vo |> Option.bind (fun v -> { Setting.defaultValue with settingPath = addParent p n; settingFloat = v } |> Some)

    let addDoubleOpt p n vo s = 
        match setDoubleOpt p n vo with
        | Some x -> x :: s
        | None -> s

    let getDoubleOpt (m : SettingMap) po n = 
        addParent po n
        |> tryFindByName m
        |> Option.bind (fun v -> v.settingFloat |> Some)


    let getTextOpt (m : SettingMap) po n = 
        addParent po n
        |> tryFindByName m
        |> Option.bind (fun v -> v.settingText)


    type DistributionParams
        with
        static member thresholdName = "threshold"
        static member scaleName = "scale"
        static member shiftName = "shift"

        static member getValue (m : SettingMap) po = 
            {
                threshold = getDoubleOpt m po DistributionParams.thresholdName
                scale = getDoubleOpt m po DistributionParams.scaleName
                shift = getDoubleOpt m po DistributionParams.shiftName
            }

        member this.setValue po s = 
            [
                setDoubleOpt po DistributionParams.thresholdName this.threshold
                setDoubleOpt po DistributionParams.scaleName this.scale
                setDoubleOpt po DistributionParams.shiftName this.shift
            ]
            |> List.choose id
            |> add s


    type Distribution
        with
        static member tryGet (m : SettingMap) (seeder : unit -> int) po =
            match getTextOpt m po DistributionName with
            | Some s ->
                match s with
                | DeltaName ->
                    let p = DistributionParams.getValue m (addParent po DeltaName)
                    DeltaDistribution (seeder(), p) |> Delta |> Some
                | BiDeltaName ->
                    let p = DistributionParams.getValue m (addParent po BiDeltaName)
                    BiDeltaDistribution (seeder(), p) |> BiDelta |> Some
                | UniformName ->
                    let p = DistributionParams.getValue m (addParent po UniformName)
                    UniformDistribution (seeder(), p) |> Uniform |> Some
                | TriangularName ->
                    let p = DistributionParams.getValue m (addParent po TriangularName)
                    TriangularDistribution (seeder(), p) |> Triangular |> Some
                | SymmetricTriangularName ->
                    let p = DistributionParams.getValue m (addParent po SymmetricTriangularName)
                    SymmetricTriangularDistribution (seeder(), p) |> SymmetricTriangular |> Some
                | _ -> None
            | None -> None

        member this.setValue po s =
            s
            |> this.distributionParams.setValue (addParent po this.name)
            |> add [ setText po DistributionName this.name ]


    type RateMultiplierDistribution
        with
        static member tryGet (m : SettingMap) (seeder : unit -> int) po =
            match getTextOpt m po RateMultiplierDistributionName with
            | Some s ->
                match s with
                | NoneRateMultName -> NoneRateMult |> Some
                | RateMultDistrName -> 
                    addParent po RateMultDistrName
                    |> Distribution.tryGet m seeder
                    |> Option.bind (fun d -> RateMultDistr d |> Some)
                | _ -> None
            | None -> None

        member this.setValue po s =
            match this with
            | NoneRateMult -> s
            | RateMultDistr d -> d.setValue (addParent po this.name) s
            |> add [ setText po RateMultiplierDistributionName this.name ]


    type EeDistribution
        with
        static member tryGet (m : SettingMap) (seeder : unit -> int) po =
            match getTextOpt m po EeDistributionName with
            | Some s ->
                match s with
                | EeDistributionName ->
                    addParent po EeDistributionName
                    |> Distribution.tryGet m seeder
                    |> Option.bind (fun d -> EeDistribution d |> Some)
                | _ -> None
            | None -> None

        member this.setValue po s =
            match this with
            | EeDistribution d -> d.setValue (addParent po this.name) s
            |> add [ setText po EeDistributionName this.name ]


    type CatRatesEeParam
        with
        static member rateMultiplierDistrName = "rateMultiplierDistr"
        static member eeForwardDistributionName = "eeForwardDistribution"
        static member eeBackwardDistributionName = "eeBackwardDistribution"

        static member tryGet (m : SettingMap) (seeder : unit -> int) po =
            match addParent po CatRatesEeParam.rateMultiplierDistrName |> RateMultiplierDistribution.tryGet m seeder with
            | Some r ->
                {
                    rateMultiplierDistr = r
                    eeForwardDistribution = addParent po CatRatesEeParam.eeForwardDistributionName |> EeDistribution.tryGet m seeder
                    eeBackwardDistribution= addParent po CatRatesEeParam.eeBackwardDistributionName |> EeDistribution.tryGet m seeder
                }
                |> Some
            | None -> None

        member this.setValue po s = 
            let setEeOpt (eo : EeDistribution option) p sx = 
                match eo with
                | Some e -> e.setValue p sx
                | None -> sx

            s
            |> this.rateMultiplierDistr.setValue (addParent po CatRatesEeParam.rateMultiplierDistrName)
            |> setEeOpt this.eeForwardDistribution (addParent po CatRatesEeParam.eeForwardDistributionName)
            |> setEeOpt this.eeBackwardDistribution (addParent po CatRatesEeParam.eeBackwardDistributionName)


    type RateMultiplierDistributionGetter
        with
        static member tryGet (m : SettingMap) po =
            match getTextOpt m po RateMultiplierDistributionGetterName with
            | Some s -> 
                match s with
                | NoneRateMultDistrGetterName -> NoneRateMultDistrGetter |> Some
                | DeltaRateMultDistrGetterName -> DeltaRateMultDistrGetter |> Some
                | TriangularRateMultDistrGetterName -> TriangularRateMultDistrGetter |> Some
                | SymmetricTriangularRateMultDistrGetterName -> SymmetricTriangularRateMultDistrGetter |> Some
                | _ -> None
            | None -> None

        member this.setValue po s =
            s
            |> add [ setText po RateMultiplierDistributionGetterName this.name ]


    type EeDistributionGetter
        with
        static member tryGet (m : SettingMap) po =
            match getTextOpt m po EeDistributionGetterName with
            | Some s -> 
                match s with
                | NoneEeGetterName -> NoneEeGetter |> Some
                | DeltaEeDistributionGetterName -> DeltaEeDistributionGetter |> Some
                | CenteredEeDistributionGetterName -> CenteredEeDistributionGetter |> Some
                | _ -> None
            | None -> None

        member this.setValue po s =
            s
            |> add [ setText po EeDistributionGetterName this.name ]


    type CatRatesSimilarityParam
        with
        static member simBaseDistributionName = "simBaseDistribution"
        static member getRateMultiplierDistrName = "getRateMultiplierDistr"
        static member getForwardEeDistrName = "getForwardEeDistr"
        static member getBackwardEeDistrName = "getBackwardEeDistr"

        static member tryGet (m : SettingMap) (seeder : unit -> int) po = 
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

        member this.setValue po s =
            s
            |> this.simBaseDistribution.setValue (addParent po CatRatesSimilarityParam.simBaseDistributionName)
            |> this.getRateMultiplierDistr.setValue (addParent po CatRatesSimilarityParam.getRateMultiplierDistrName)
            |> this.getForwardEeDistr.setValue (addParent po CatRatesSimilarityParam.getForwardEeDistrName)
            |> this.getBackwardEeDistr.setValue (addParent po CatRatesSimilarityParam.getBackwardEeDistrName)


    type FoodCreationParam
        with
        static member foodCreationRateName = "foodCreationRate"

        static member tryGet (m : SettingMap) po = 
            match getDoubleOpt m po FoodCreationParam.foodCreationRateName with
            | Some v ->
                {
                    foodCreationRate = v
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> add [ setDouble po FoodCreationParam.foodCreationRateName this.foodCreationRate ]


    type WasteRemovalParam
        with
        static member wasteRemovalRateName = "wasteRemovalRate"

        static member tryGet (m : SettingMap) po = 
            match getDoubleOpt m po WasteRemovalParam.wasteRemovalRateName with
            | Some v -> 
                {
                    wasteRemovalRate = v
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> add [ setDouble po WasteRemovalParam.wasteRemovalRateName this.wasteRemovalRate ]


    type WasteRecyclingParam
        with
        static member wasteRecyclingRateName = "wasteRecyclingRate"

        static member tryGet (m : SettingMap) po = 
            match getDoubleOpt m po WasteRecyclingParam.wasteRecyclingRateName with
            | Some v -> 
                {
                    wasteRecyclingRate = v
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> add [ setDouble po WasteRecyclingParam.wasteRecyclingRateName this.wasteRecyclingRate ]


    type SynthesisRandomParam
        with
        static member synthesisDistributionName = "synthesisDistribution"
        static member forwardScaleName = "forwardScale"
        static member backwardScaleName = "backwardScale"

        static member tryGet (m : SettingMap) (seeder : unit -> int) po = 
            match addParent po SynthesisRandomParam.synthesisDistributionName |> Distribution.tryGet m seeder with
            | Some d ->
                {
                    synthesisDistribution = d
                    forwardScale = getDoubleOpt m po SynthesisRandomParam.forwardScaleName
                    backwardScale = getDoubleOpt m po SynthesisRandomParam.backwardScaleName
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> this.synthesisDistribution.setValue (addParent po SynthesisRandomParam.synthesisDistributionName)
            |> addDoubleOpt po SynthesisRandomParam.forwardScaleName this.forwardScale
            |> addDoubleOpt po SynthesisRandomParam.backwardScaleName this.backwardScale


    type SynthesisParam
        with
        static member tryGet (m : SettingMap) (seeder : unit -> int) po =
            match getTextOpt m po SynthesisParamName with
            | Some s -> 
                match s with
                | SynthRndParamName -> 
                    addParent po SynthRndParamName
                    |> SynthesisRandomParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> SynthRndParam |> Some)
                | _ -> None
            | None -> None

        member this.setValue po s =
            match this with
            | SynthRndParam d -> d.setValue (addParent po this.name) s
            |> add [ setText po SynthRndParamName this.name ]

    type CatalyticSynthesisRandomParam
        with
        static member catSynthRndEeParamsName = "catSynthRndEeParams"

        static member tryGet (m : SettingMap) (seeder : unit -> int) po = 
            match addParent po CatalyticSynthesisRandomParam.catSynthRndEeParamsName |> CatRatesEeParam.tryGet m seeder with
            | Some d ->
                {
                    catSynthRndEeParams = d
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> this.catSynthRndEeParams.setValue (addParent po CatalyticSynthesisRandomParam.catSynthRndEeParamsName)


    type CatalyticSynthesisParam
        with
        static member tryGet (m : SettingMap) (seeder : unit -> int) po =
            match getTextOpt m po CatalyticSynthesisParamName with
            | Some s -> 
                match s with
                | CatSynthRndParamName ->
                    addParent po CatSynthRndParamName
                    |> CatalyticSynthesisRandomParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> CatSynthRndParam |> Some)
                | CatSynthSimParamName -> 
                    addParent po CatSynthSimParamName
                    |> CatRatesSimilarityParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> CatSynthSimParam |> Some)
                | _ -> None
            | None -> None

        member this.setValue po s =
            match this with
            | CatSynthRndParam d -> d.setValue (addParent po this.name) s
            | CatSynthSimParam d -> d.setValue (addParent po this.name) s
            |> add [ setText po CatalyticSynthesisParamName this.name ]


    type DestructionRandomParam
        with
        static member destructionDistributionName = "destructionDistribution"
        static member forwardScaleName = "forwardScale"
        static member backwardScaleName = "backwardScale"

        static member tryGet (m : SettingMap) (seeder : unit -> int) po = 
            match addParent po DestructionRandomParam.destructionDistributionName |> Distribution.tryGet m seeder with
            | Some d ->
                {
                    destructionDistribution = d
                    forwardScale = getDoubleOpt m po DestructionRandomParam.forwardScaleName
                    backwardScale = getDoubleOpt m po DestructionRandomParam.backwardScaleName
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> this.destructionDistribution.setValue (addParent po DestructionRandomParam.destructionDistributionName)
            |> addDoubleOpt po DestructionRandomParam.forwardScaleName this.forwardScale
            |> addDoubleOpt po DestructionRandomParam.backwardScaleName this.backwardScale


// here
    type DestructionParam
        with
        static member className = "DestructionParam"

        static member tryGet (m : SettingMap) (seeder : unit -> int) po =
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

        static member tryGet (m : SettingMap) (seeder : unit -> int) po = 
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

        static member tryGet (m : SettingMap) (seeder : unit -> int) po =
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

        static member tryGet (m : SettingMap) (seeder : unit -> int) po = 
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

        static member tryGet (m : SettingMap) (seeder : unit -> int) po =
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

        static member tryGet (m : SettingMap) (seeder : unit -> int) po = 
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

        static member tryGet (m : SettingMap) (seeder : unit -> int) po =
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

        static member tryGet (m : SettingMap) (seeder : unit -> int) po = 
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

        static member tryGet (m : SettingMap) (seeder : unit -> int) po =
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

        static member tryGet (m : SettingMap) (seeder : unit -> int) po = 
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

        static member tryGet (m : SettingMap) (seeder : unit -> int) po =
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

        static member tryGet (m : SettingMap) (seeder : unit -> int) po = 
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

        static member tryGet (m : SettingMap) (seeder : unit -> int) po =
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

        static member tryGet (m : SettingMap) (seeder : unit -> int) po = 
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

        static member tryGet (m : SettingMap) (seeder : unit -> int) po =
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
