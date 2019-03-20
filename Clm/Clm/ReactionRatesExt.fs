namespace Clm

open ReactionRates

module ReactionRatesExt =

    type FoodCreationParam
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | FoodCreationRateParam d -> Some (p.usage, d)
            | _ -> None


    type WasteRemovalParam
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | WasteRemovalRateParam d -> Some (p.usage, d)
            | _ -> None


    type WasteRecyclingParam
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | WasteRecyclingRateParam d -> Some (p.usage, d)
            | _ -> None


    type SedimentationDirectRandomParam
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | SedimentationDirectRateParam (SedDirRndParam d) -> Some (p.usage, d)
            | _ -> None


    type SedDirSimilarityParam
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | SedimentationDirectRateParam (SedDirSimParam d) -> Some (p.usage, d)
            | _ -> None


    type SedimentationAllRandomParam
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | SedimentationAllRateParam (SedAllRndParam d) -> Some (p.usage, d)
            | _ -> None


    type SynthesisRandomParam
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | SynthesisRateParam (SynthRndParam d) -> Some (p.usage, d)
            | _ -> None


    type CatalyticSynthesisRandomParam
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | CatalyticSynthesisRateParam (CatSynthRndParam d) -> Some (p.usage, d)
            | _ -> None


    type DestructionRandomParam
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | DestructionRateParam (DestrRndParam d) -> Some (p.usage, d)
            | _ -> None


    type CatalyticDestructionRandomParam
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | CatalyticDestructionRateParam (CatDestrRndParam d) -> Some (p.usage, d)
            | _ -> None


    type LigationRandomParam
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | LigationRateParam (LigRndParam d) -> Some (p.usage, d)
            | _ -> None


    type CatalyticLigationRandomParam
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | CatalyticLigationRateParam (CatLigRndParam d) -> Some (p.usage, d)
            | _ -> None


    type RacemizationRandomParam
        with
        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | RacemizationRateParam (RacemRndParam d) -> Some (p.usage, d)
            | _ -> None


    type CatalyticRacemizationRandomParam
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | CatalyticRacemizationRateParam (CatRacemRndParam d) -> Some (p.usage, d)
            | _ -> None
