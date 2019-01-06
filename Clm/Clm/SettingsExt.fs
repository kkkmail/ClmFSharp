namespace Clm

open Clm.Substances
open Clm.Distributions
open Clm.ReactionRates
open Clm.DataLocation
open Clm.ModelParams

module SettingsExt =

    type Seeder = int option -> int

    let tryFindByName (m : SettingMap) n = m.TryFind n
    let addParent p n = p @ [ (n, 0) ]
    let add a b = a @ b


    let addOpt ao b =
        match ao with
        | Some a -> a @ b
        | None -> b


    let setDouble p n v = { Setting.defaultValue() with settingPath = addParent p n; settingFloat = v }
    let setInt p n v = { Setting.defaultValue() with settingPath = addParent p n; settingLong = int64 v }
    let setIntOpt p n vo = vo |> Option.bind (fun v -> setInt p n v |> Some)
    let setText p n v = { Setting.defaultValue() with settingPath = addParent p n; settingText = Some v }
    let setTextOpt p n vo = vo |> Option.bind (fun v -> setText p n v |> Some)
    let setBool p n v = { Setting.defaultValue() with settingPath = addParent p n; settingBit = v }
    let setDoubleOpt p n vo = vo |> Option.bind (fun v -> setDouble p n v |> Some)
    let setBoolOpt p n vo = vo |> Option.bind (fun v -> setBool p n v |> Some)
    let setLong p n v = { Setting.defaultValue() with settingPath = addParent p n; settingLong = v }


    let addDoubleOpt p n vo s = 
        match setDoubleOpt p n vo with
        | Some x -> x :: s
        | None -> s


    let getDoubleOpt (m : SettingMap) po n =
        addParent po n
        |> tryFindByName m
        |> Option.bind (fun v -> v.settingFloat |> Some)


    let getIntOpt (m : SettingMap) po n =
        addParent po n
        |> tryFindByName m
        |> Option.bind (fun v -> v.settingLong |> int |> Some)


    let getLongOpt (m : SettingMap) po n =
        addParent po n
        |> tryFindByName m
        |> Option.bind (fun v -> v.settingLong |> Some)


    let getTextOpt (m : SettingMap) po n =
        addParent po n
        |> tryFindByName m
        |> Option.bind (fun v -> v.settingText)


    let getBoolOpt (m : SettingMap) po n =
        addParent po n
        |> tryFindByName m
        |> Option.bind (fun v -> v.settingBit |> Some)


    let getBool (m : SettingMap) po n =
        match getBoolOpt m po n with
        | Some b -> b
        | None -> false


    type NumberOfAminoAcids
        with
        static member tryGet (m : SettingMap) po =
            getIntOpt m po NumberOfAminoAcidsName |> Option.bind (fun i -> NumberOfAminoAcids.tryCreate i)

        member this.setValue po s =
            add s [ setInt po NumberOfAminoAcidsName this.length]


    type MaxPeptideLength
        with
        static member tryGet (m : SettingMap) po =
            getIntOpt m po MaxPeptideLengthName |> Option.bind (fun i -> MaxPeptideLength.tryCreate i)

        member this.setValue po s =
            add s [ setInt po MaxPeptideLengthName this.length]


    [<Literal>]
    let thresholdName = "threshold"

    [<Literal>]
    let scaleName = "scale"

    [<Literal>]
    let shiftName = "shift"

    type DistributionParams
        with
        static member getValue (m : SettingMap) po =
            {
                threshold = getDoubleOpt m po thresholdName
                scale = getDoubleOpt m po scaleName
                shift = getDoubleOpt m po shiftName
            }

        member this.setValue po s =
            [
                setDoubleOpt po thresholdName this.threshold
                setDoubleOpt po scaleName this.scale
                setDoubleOpt po shiftName this.shift
            ]
            |> List.choose id
            |> add s


    [<Literal>]
    let seedValueName = "seedValue"

    type Distribution
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
            match getTextOpt m po DistributionName with
            | Some s ->
                let v = getIntOpt m po seedValueName

                match s with
                | DeltaName ->
                    let p = DistributionParams.getValue m (addParent po DeltaName)
                    DeltaDistribution (seeder v, p) |> Delta |> Some
                | BiDeltaName ->
                    let p = DistributionParams.getValue m (addParent po BiDeltaName)
                    BiDeltaDistribution (seeder v, p) |> BiDelta |> Some
                | UniformName ->
                    let p = DistributionParams.getValue m (addParent po UniformName)
                    UniformDistribution (seeder v, p) |> Uniform |> Some
                | TriangularName ->
                    let p = DistributionParams.getValue m (addParent po TriangularName)
                    TriangularDistribution (seeder v, p) |> Triangular |> Some
                | SymmetricTriangularName ->
                    let p = DistributionParams.getValue m (addParent po SymmetricTriangularName)
                    SymmetricTriangularDistribution (seeder v, p) |> SymmetricTriangular |> Some
                | _ -> None
            | None -> None

        member this.setValue po s =
            s
            |> this.distributionParams.setValue (addParent po this.name)
            |> add [ setText po DistributionName this.name ]
            |> add [ setInt po seedValueName this.seedValue ]


    type RateMultiplierDistribution
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
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
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
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


    [<Literal>]
    let rateMultiplierDistrName = "rateMultiplierDistr"


    [<Literal>]
    let  eeForwardDistributionName = "eeForwardDistribution"


    [<Literal>]
    let  eeBackwardDistributionName = "eeBackwardDistribution"


    type CatRatesEeParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
            match addParent po rateMultiplierDistrName |> RateMultiplierDistribution.tryGet m seeder with
            | Some r ->
                {
                    rateMultiplierDistr = r
                    eeForwardDistribution = addParent po eeForwardDistributionName |> EeDistribution.tryGet m seeder
                    eeBackwardDistribution= addParent po eeBackwardDistributionName |> EeDistribution.tryGet m seeder
                }
                |> Some
            | None -> None

        member this.setValue po s = 
            let setEeOpt (eo : EeDistribution option) p sx = 
                match eo with
                | Some e -> e.setValue p sx
                | None -> sx

            s
            |> this.rateMultiplierDistr.setValue (addParent po rateMultiplierDistrName)
            |> setEeOpt this.eeForwardDistribution (addParent po eeForwardDistributionName)
            |> setEeOpt this.eeBackwardDistribution (addParent po eeBackwardDistributionName)


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


    [<Literal>]
    let simBaseDistributionName = "simBaseDistribution"

    [<Literal>]
    let getRateMultiplierDistrName = "getRateMultiplierDistr"

    [<Literal>]
    let getForwardEeDistrName = "getForwardEeDistr"

    [<Literal>]
    let getBackwardEeDistrName = "getBackwardEeDistr"

    type CatRatesSimilarityParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po = 
            let d() = addParent po simBaseDistributionName |> Distribution.tryGet m seeder
            let r() = addParent po getRateMultiplierDistrName |> RateMultiplierDistributionGetter.tryGet m
            let f() = addParent po getForwardEeDistrName |> EeDistributionGetter.tryGet m
            let b() = addParent po getBackwardEeDistrName |> EeDistributionGetter.tryGet m

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
            |> this.simBaseDistribution.setValue (addParent po simBaseDistributionName)
            |> this.getRateMultiplierDistr.setValue (addParent po getRateMultiplierDistrName)
            |> this.getForwardEeDistr.setValue (addParent po getForwardEeDistrName)
            |> this.getBackwardEeDistr.setValue (addParent po getBackwardEeDistrName)


    [<Literal>]
    let foodCreationRateName = "foodCreationRate"


    type FoodCreationParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po = 
            match getDoubleOpt m po foodCreationRateName with
            | Some v ->
                {
                    foodCreationRate = v
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> add [ setDouble po foodCreationRateName this.foodCreationRate ]


    [<Literal>]
    let wasteRemovalRateName = "wasteRemovalRate"


    type WasteRemovalParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po = 
            match getDoubleOpt m po wasteRemovalRateName with
            | Some v -> 
                {
                    wasteRemovalRate = v
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> add [ setDouble po wasteRemovalRateName this.wasteRemovalRate ]


    [<Literal>]
    let wasteRecyclingRateName = "wasteRecyclingRate"

    type WasteRecyclingParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
            match getDoubleOpt m po wasteRecyclingRateName with
            | Some v -> 
                {
                    wasteRecyclingRate = v
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> add [ setDouble po wasteRecyclingRateName this.wasteRecyclingRate ]


    [<Literal>]
    let synthesisDistributionName = "synthesisDistribution"

    [<Literal>]
    let forwardScaleName = "forwardScale"

    [<Literal>]
    let backwardScaleName = "backwardScale"


    type SynthesisRandomParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po = 
            match addParent po synthesisDistributionName |> Distribution.tryGet m seeder with
            | Some d ->
                {
                    synthesisDistribution = d
                    forwardScale = getDoubleOpt m po forwardScaleName
                    backwardScale = getDoubleOpt m po backwardScaleName
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> this.synthesisDistribution.setValue (addParent po synthesisDistributionName)
            |> addDoubleOpt po forwardScaleName this.forwardScale
            |> addDoubleOpt po backwardScaleName this.backwardScale


    type SynthesisParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
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
            |> add [ setText po SynthesisParamName this.name ]


    [<Literal>]
    let catSynthRndEeParamsName = "catSynthRndEeParams"


    type CatalyticSynthesisRandomParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po = 
            match addParent po catSynthRndEeParamsName |> CatRatesEeParam.tryGet m seeder with
            | Some d ->
                {
                    catSynthRndEeParams = d
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> this.catSynthRndEeParams.setValue (addParent po catSynthRndEeParamsName)


    type CatalyticSynthesisParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
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


    [<Literal>]
    let destructionDistributionName = "destructionDistribution"


    type DestructionRandomParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po = 
            match addParent po destructionDistributionName |> Distribution.tryGet m seeder with
            | Some d ->
                {
                    destructionDistribution = d
                    forwardScale = getDoubleOpt m po forwardScaleName
                    backwardScale = getDoubleOpt m po backwardScaleName
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> this.destructionDistribution.setValue (addParent po destructionDistributionName)
            |> addDoubleOpt po forwardScaleName this.forwardScale
            |> addDoubleOpt po backwardScaleName this.backwardScale


    type DestructionParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
            match getTextOpt m po DestructionParamName with
            | Some s -> 
                match s with
                | DestrRndParamName -> 
                    addParent po DestrRndParamName
                    |> DestructionRandomParam.tryGet m seeder
                    |> Option.bind (fun e -> e |> DestrRndParam |> Some)
                | _ -> None
            | None -> None

        member this.setValue po s =
            match this with
            | DestrRndParam d -> d.setValue (addParent po this.name) s
            |> add [ setText po DestructionParamName this.name ]


    [<Literal>]
    let catDestrRndEeParamsName = "catDestrRndEeParams"


    type CatalyticDestructionRandomParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po = 
            match addParent po catDestrRndEeParamsName |> CatRatesEeParam.tryGet m seeder with
            | Some d ->
                {
                    catDestrRndEeParams = d
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> this.catDestrRndEeParams.setValue (addParent po catDestrRndEeParamsName)


    type CatalyticDestructionParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
            match getTextOpt m po CatalyticDestructionParamName with
            | Some s -> 
                match s with
                | CatDestrRndParamName -> 
                    addParent po CatDestrRndParamName 
                    |> CatalyticDestructionRandomParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> CatDestrRndParam |> Some)
                | CatDestrSimParamName -> 
                    addParent po CatDestrSimParamName 
                    |> CatRatesSimilarityParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> CatDestrSimParam |> Some)
                | _ -> None
            | None -> None

        member this.setValue po s =
            match this with
            | CatDestrRndParam d -> d.setValue (addParent po this.name) s
            | CatDestrSimParam d -> d.setValue (addParent po this.name) s
            |> add [ setText po CatalyticDestructionParamName this.name ]


    [<Literal>]
    let sedimentationDirectDistributionName = "sedimentationDirectDistribution"


    type SedimentationDirectRandomParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po = 
            match addParent po sedimentationDirectDistributionName |> Distribution.tryGet m seeder with
            | Some d ->
                {
                    sedimentationDirectDistribution = d
                    forwardScale = getDoubleOpt m po forwardScaleName
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> this.sedimentationDirectDistribution.setValue (addParent po sedimentationDirectDistributionName)
            |> addDoubleOpt po forwardScaleName this.forwardScale


    type SedimentationDirectParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
            match getTextOpt m po SedimentationDirectParamName with
            | Some s -> 
                match s with
                | SedDirRndParamName -> 
                    addParent po SedDirRndParamName
                    |> SedimentationDirectRandomParam.tryGet m seeder
                    |> Option.bind (fun e -> e |> SedDirRndParam |> Some)
                | _ -> None
            | None -> None

        member this.setValue po s =
            match this with
            | SedDirRndParam d -> d.setValue (addParent po this.name) s
            |> add [ setText po SedimentationDirectParamName this.name ]


    [<Literal>]
    let sedimentationAllDistributionName = "sedimentationAllDistribution"


    type SedimentationAllRandomParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po = 
            match addParent po sedimentationAllDistributionName |> Distribution.tryGet m seeder with
            | Some d ->
                {
                    sedimentationAllDistribution = d
                    forwardScale = getDoubleOpt m po forwardScaleName
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> this.sedimentationAllDistribution.setValue (addParent po sedimentationAllDistributionName)
            |> addDoubleOpt po forwardScaleName this.forwardScale


    type SedimentationAllParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
            match getTextOpt m po SedimentationAllParamName with
            | Some s -> 
                match s with
                | SedAllRndParamName -> 
                    addParent po SedAllRndParamName
                    |> SedimentationAllRandomParam.tryGet m seeder
                    |> Option.bind (fun e -> e |> SedAllRndParam |> Some)
                | _ -> None
            | None -> None

        member this.setValue po s =
            match this with
            | SedAllRndParam d -> d.setValue (addParent po this.name) s
            |> add [ setText po SedimentationAllParamName this.name ]


    [<Literal>]
    let ligationDistributionName = "ligationDistribution"


    type LigationRandomParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po = 
            match addParent po ligationDistributionName |> Distribution.tryGet m seeder with
            | Some d ->
                {
                    ligationDistribution = d
                    forwardScale = getDoubleOpt m po forwardScaleName
                    backwardScale = getDoubleOpt m po backwardScaleName
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> this.ligationDistribution.setValue (addParent po ligationDistributionName)
            |> addDoubleOpt po forwardScaleName this.forwardScale
            |> addDoubleOpt po backwardScaleName this.backwardScale


    type LigationParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
            match getTextOpt m po LigationParamName with
            | Some s -> 
                match s with
                | LigRndParamName ->
                    addParent po LigRndParamName
                    |> LigationRandomParam.tryGet m seeder
                    |> Option.bind (fun e -> e |> LigRndParam |> Some)
                | _ -> None
            | None -> None

        member this.setValue po s =
            match this with
            | LigRndParam d -> d.setValue (addParent po this.name) s
            |> add [ setText po LigationParamName this.name ]


    [<Literal>]
    let catLigRndEeParamsName = "catLigRndEeParams"

    type CatalyticLigationRandomParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po = 
            match addParent po catLigRndEeParamsName |> CatRatesEeParam.tryGet m seeder with
            | Some d ->
                {
                    catLigRndEeParams = d
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> this.catLigRndEeParams.setValue (addParent po catLigRndEeParamsName)


    type CatalyticLigationParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
            match getTextOpt m po CatalyticLigationParamName with
            | Some s -> 
                match s with
                | CatLigRndParamName -> 
                    addParent po CatLigRndParamName
                    |> CatalyticLigationRandomParam.tryGet m seeder
                    |> Option.bind (fun e -> e |> CatLigRndParam |> Some)
                | _ -> None
            | None -> None

        member this.setValue po s =
            match this with
            | CatLigRndParam d -> d.setValue (addParent po this.name) s
            |> add [ setText po CatalyticLigationParamName this.name ]


    [<Literal>]
    let racemizationDistributionName = "racemizationDistribution"


    type RacemizationRandomParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po = 
            match addParent po racemizationDistributionName |> Distribution.tryGet m seeder with
            | Some d ->
                {
                    racemizationDistribution = d
                    forwardScale = getDoubleOpt m po forwardScaleName
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> this.racemizationDistribution.setValue (addParent po racemizationDistributionName)
            |> addDoubleOpt po forwardScaleName this.forwardScale


    type RacemizationParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
            match getTextOpt m po RacemizationParamName with
            | Some s -> 
                match s with
                | RacemRndParamName -> 
                    addParent po RacemRndParamName
                    |> RacemizationRandomParam.tryGet m seeder
                    |> Option.bind (fun e -> e |> RacemRndParam |> Some)
                | _ -> None
            | None -> None

        member this.setValue po s =
            match this with
            | RacemRndParam d -> d.setValue (addParent po this.name) s
            |> add [ setText po RacemizationParamName this.name ]


    [<Literal>]
    let catRacemRndEeParamsName = "catRacemRndEeParams"


    type CatalyticRacemizationRandomParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po = 
            match addParent po catRacemRndEeParamsName |> CatRatesEeParam.tryGet m seeder with
            | Some d ->
                {
                    catRacemRndEeParams = d
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> this.catRacemRndEeParams.setValue (addParent po catRacemRndEeParamsName)


    type CatalyticRacemizationParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
            match getTextOpt m po CatalyticRacemizationParamName with
            | Some s -> 
                match s with
                | CatRacemRndParamName ->
                    addParent po CatRacemRndParamName 
                    |> CatalyticRacemizationRandomParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> CatRacemRndParam |> Some)
                | CatRacemSimParamName ->
                    addParent po CatRacemSimParamName
                    |> CatRatesSimilarityParam.tryGet m seeder
                    |> Option.bind (fun e -> e |> CatRacemSimParam |> Some)
                | _ -> None
            | None -> None

        member this.setValue po s =
            match this with
            | CatRacemRndParam d -> d.setValue (addParent po this.name) s
            | CatRacemSimParam d -> d.setValue (addParent po this.name) s
            |> add [ setText po CatalyticRacemizationParamName this.name ]


    // Generated. See ReactonRateModelParamGenerator.xlsx
    type ReactionRateModelParam
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
            match getTextOpt m po ReactionRateModelParamName with
            | Some s -> 
                match s with
                | FoodCreationRateParamName ->
                    addParent po FoodCreationRateParamName
                    |> FoodCreationParam.tryGet m seeder
                    |> Option.bind (fun e -> e |> FoodCreationRateParam |> Some)
                | WasteRemovalRateParamName ->
                    addParent po WasteRemovalRateParamName
                    |> WasteRemovalParam.tryGet m seeder
                    |> Option.bind (fun e -> e |> WasteRemovalRateParam |> Some)
                | WasteRecyclingRateParamName ->
                    addParent po WasteRecyclingRateParamName
                    |> WasteRecyclingParam.tryGet m seeder
                    |> Option.bind (fun e -> e |> WasteRecyclingRateParam |> Some)
                | SynthesisRateParamName ->
                    addParent po SynthesisRateParamName 
                    |> SynthesisParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> SynthesisRateParam |> Some)
                | DestructionRateParamName ->
                    addParent po DestructionRateParamName 
                    |> DestructionParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> DestructionRateParam |> Some)
                | CatalyticSynthesisRateParamName ->
                    addParent po CatalyticSynthesisRateParamName 
                    |> CatalyticSynthesisParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> CatalyticSynthesisRateParam |> Some)
                | CatalyticDestructionRateParamName ->
                    addParent po CatalyticDestructionRateParamName 
                    |> CatalyticDestructionParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> CatalyticDestructionRateParam |> Some)
                | LigationRateParamName ->
                    addParent po LigationRateParamName 
                    |> LigationParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> LigationRateParam |> Some)
                | CatalyticLigationRateParamName ->
                    addParent po CatalyticLigationRateParamName 
                    |> CatalyticLigationParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> CatalyticLigationRateParam |> Some)
                | SedimentationDirectRateParamName ->
                    addParent po SedimentationDirectRateParamName 
                    |> SedimentationDirectParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> SedimentationDirectRateParam |> Some)
                | SedimentationAllRateParamName ->
                    addParent po SedimentationAllRateParamName 
                    |> SedimentationAllParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> SedimentationAllRateParam |> Some)
                | RacemizationRateParamName ->
                    addParent po RacemizationRateParamName 
                    |> RacemizationParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> RacemizationRateParam |> Some)
                | CatalyticRacemizationRateParamName ->
                    addParent po CatalyticRacemizationRateParamName 
                    |> CatalyticRacemizationParam.tryGet m seeder 
                    |> Option.bind (fun e -> e |> CatalyticRacemizationRateParam |> Some)
                | _ -> None
            | None -> None

        member this.setValue po s =
            match this with
            | FoodCreationRateParam d -> d.setValue (addParent po this.name) s
            | WasteRemovalRateParam d -> d.setValue (addParent po this.name) s
            | WasteRecyclingRateParam d -> d.setValue (addParent po this.name) s
            | SynthesisRateParam d -> d.setValue (addParent po this.name) s
            | DestructionRateParam d -> d.setValue (addParent po this.name) s
            | CatalyticSynthesisRateParam d -> d.setValue (addParent po this.name) s
            | CatalyticDestructionRateParam d -> d.setValue (addParent po this.name) s
            | LigationRateParam d -> d.setValue (addParent po this.name) s
            | CatalyticLigationRateParam d -> d.setValue (addParent po this.name) s
            | SedimentationDirectRateParam d -> d.setValue (addParent po this.name) s
            | SedimentationAllRateParam d -> d.setValue (addParent po this.name) s
            | RacemizationRateParam d -> d.setValue (addParent po this.name) s
            | CatalyticRacemizationRateParam d -> d.setValue (addParent po this.name) s
            |> add [ setText po ReactionRateModelParamName this.name ]


    type ReactionRateModelParamUsage
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
            match getTextOpt m po ReactionRateModelParamUsageName with
            | Some s ->
                match s with
                | PrimaryParamName -> Some PrimaryParam
                | DependsOnParamName -> Some DependsOnParam
                | _ -> None
            | None -> None

        member this.setValue po s =
            add [ setText po ReactionRateModelParamUsageName this.name ] s


    [<Literal>]
    let modelParamName = "modelParam"

    [<Literal>]
    let usageParamName = "usage"

    type ReactionRateModelParamWithUsage
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
            let p() = addParent po modelParamName |> ReactionRateModelParam.tryGet m seeder
            let u() = addParent po usageParamName |> ReactionRateModelParamUsage.tryGet m seeder

            match p(), u() with
            | Some a, Some b ->
                {
                    modelParam = a
                    usage = b
                }
                |> Some
            | _ -> None

        static member getAll (m : SettingMap) (seeder : Seeder) po =
            ReactionRateModelParam.allVariableParamNames
            |> List.map (fun e -> ReactionRateModelParamWithUsage.tryGet m seeder (addParent po e) )
            |> List.choose id
            |> List.sort

        member this.setValue po s =
            s
            |> this.modelParam.setValue (addParent po modelParamName)
            |> this.usage.setValue (addParent po usageParamName)

        static member setAll (rates : list<ReactionRateModelParamWithUsage>) po s =
            rates |> List.fold (fun acc e -> e.setValue (addParent po e.modelParam.variableParamName) acc) s


    [<Literal>]
    let fileStructureVersionNumberName = "fileStructureVersionNumber"

    [<Literal>]
    let versionNumberName = "versionNumber"

    [<Literal>]
    let modelNameName = "modelName"

    [<Literal>]
    let numberOfSubstancesName = "numberOfSubstances"

    [<Literal>]
    let numberOfAminoAcidsName = "numberOfAminoAcids"

    [<Literal>]
    let maxPeptideLengthName = "maxPeptideLength"

    [<Literal>]
    let updateAllModelsName = "updateAllModels"

    [<Literal>]
    let allResultsFileName = "allResultsFile"

    type ModelInfo
        with
        static member tryGet (m : SettingMap) po =
            let a() = getTextOpt m po fileStructureVersionNumberName
            let b() = getTextOpt m po versionNumberName
            let c() = getIntOpt m po seedValueName
            let d() = getTextOpt m po modelNameName
            let e() = getIntOpt m po numberOfSubstancesName
            let f() = getIntOpt m po numberOfAminoAcidsName
            let g() = getIntOpt m po maxPeptideLengthName
            let h() = getBoolOpt m po updateAllModelsName
            let i() = getTextOpt m po allResultsFileName

            match a(), b(), c(), d(), e(), f(), g(), h(), i() with
            | Some a1, Some b1, Some c1, Some d1, Some e1, Some f1, Some g1, Some h1, Some i1 ->
                match NumberOfAminoAcids.tryCreate f1, MaxPeptideLength.tryCreate g1 with
                | Some f2, Some g2 ->
                    {
                        fileStructureVersionNumber = a1
                        versionNumber = b1
                        seedValue = c1
                        modelName = d1
                        numberOfSubstances = e1
                        numberOfAminoAcids = f2
                        maxPeptideLength = g2
                        updateAllModels = h1
                        allResultsFile = i1
                    }
                    |> Some
                | _ -> None
            | _ -> None

        member this.setValue po s =
            [
                setText po fileStructureVersionNumberName this.fileStructureVersionNumber
                setText po versionNumberName this.versionNumber
                setInt po seedValueName this.seedValue
                setText po modelNameName this.modelName
                setInt po numberOfSubstancesName this.numberOfSubstances
                setInt po numberOfAminoAcidsName this.numberOfAminoAcids.length
                setInt po maxPeptideLengthName this.maxPeptideLength.length
                setBool po updateAllModelsName this.updateAllModels
                setText po allResultsFileName this.allResultsFile
            ]
            |> add s


    //type ModelDataParams =
    //    {
    //        modelInfo : ModelInfo
    //        allParams : list<ReactionRateModelParamWithUsage>
    //    }

    [<Literal>]
    let modelInfoName = "modelInfo"

    [<Literal>]
    let allParamsName = "allParams"

    type ModelDataParams
        with
        static member tryGet (m : SettingMap) (seeder : Seeder) po =
            match ModelInfo.tryGet m (addParent po modelInfoName) with
            | Some a ->
                {
                    modelInfo = a
                    allParams = ReactionRateModelParamWithUsage.getAll m seeder (addParent po allParamsName)
                }
                |> Some
            | None -> None

        member this.setValue po s =
            s
            |> this.modelInfo.setValue (addParent po modelInfoName)
            |> ReactionRateModelParamWithUsage.setAll this.allParams (addParent po allParamsName)


    type ModeName
        with
        static member tryGet (m : SettingMap) po =
            match getTextOpt m po ModeNameName with
            | Some s -> 
                match s with
                | GenerateNameName -> Some GenerateName
                | ConsecutiveNameName ->
                    getLongOpt m po ConsecutiveNameName
                    |> Option.bind (fun n -> n |> ConsecutiveName |> Some)
                | _ -> None
            | None -> None

        member this.setValue po s =
            match this with
            | GenerateName -> []
            | ConsecutiveName n -> [ setLong po ConsecutiveNameName n ]
            |> add [ setText po ModeNameName this.name ]
            |> add s


    [<Literal>]
    let startingFolderName = "startingFolder"

    [<Literal>]
    let separatorName = "separator"

    [<Literal>]
    let padLengthName = "padLength"

    [<Literal>]
    let allModelsFileName = "allModelsFile"

    //[<Literal>]
    //let allResultsFileName = "allResultsFile"

    //[<Literal>]
    //let modelNameName = "modelName"

    [<Literal>]
    let useDefaultModeDataName = "useDefaultModeData"

    type ModelLocationInputData
        with
        static member tryGet (m : SettingMap) po =
            let a() = getTextOpt m po startingFolderName
            let b() = getTextOpt m po separatorName
            let c() = getIntOpt m po scaleName
            let d() = getTextOpt m po allModelsFileName
            let e() = getTextOpt m po allResultsFileName
            let f() = ModeName.tryGet m (addParent po modelNameName)

            match a(), b(), c(), d(), e(), f() with
            | Some a1, Some b1, Some c1, Some d1, Some e1, Some f1 ->
                {
                    startingFolder = a1
                    separator = b1
                    padLength = c1
                    allModelsFile = d1
                    allResultsFile = e1
                    modelName = f1
                    useDefaultModeData = getBool m po useDefaultModeDataName
                }
                |> Some
            | _ -> None

        member this.setValue po s =
            [
                setText po startingFolderName this.startingFolder |> Some
                setText po separatorName this.separator |> Some
                setInt po scaleName this.padLength |> Some
                setText po allModelsFileName this.allModelsFile |> Some
                setText po allResultsFileName this.allResultsFile |> Some
                setBool po useDefaultModeDataName this.useDefaultModeData |> Some
            ]
            |> List.choose id
            |> this.modelName.setValue (addParent po modelNameName)
            |> add s


    [<Literal>]
    let tEndName = "tEnd"

    [<Literal>]
    let y0Name = "y0"

    [<Literal>]
    let useAbundantName = "useAbundant"

    type ModelCommandLineParam
        with
        static member getValues (m : SettingMap) po =
            let tryGet (m : SettingMap) qo = 
                let t() = getDoubleOpt m qo tEndName
                let y() = getDoubleOpt m qo y0Name

                match t(), y() with
                | Some t1, Some y1 ->
                    {
                        tEnd = t1
                        y0 = y1
                        useAbundant = 
                            match getBoolOpt m qo useAbundantName with
                            | Some v -> v
                            | None -> false
                    }
                    |> Some
                | _ -> None

            [ for i in 0..100 -> i ]
            |> List.map (fun e -> po @ [ (ModelCommandLineParam.variableName, e) ] |> tryGet m)
            |> List.choose id

        static member setValues (p : list<ModelCommandLineParam>) po s =
            let setValue (q : ModelCommandLineParam) qo s =
                [
                    setDouble qo tEndName q.tEnd
                    setDouble qo y0Name q.y0
                    setBool qo useAbundantName q.useAbundant
                ]
                |> add s

            p
            |> List.mapi (fun i e -> (e, po @ [ (ModelCommandLineParam.variableName, i) ]))
            |> List.fold (fun acc (q, qo) -> setValue q qo acc) s
