namespace ClmImpure

open Clm.Substances
open Clm.ReactionRates
open Clm.ReactionRatesExt

open ClmImpure.ReactionRateModels

module ReactionRateModelsExt =

    let tryGetModel getter (p : list<ReactionRateModelWithUsage>) = p |> List.tryPick getter


    let tryCreateModel picker creator (p, m) =
        match tryPickParam picker p with
        | Some (u, d), q ->
            let models =
                {
                    model = creator d
                    usage = u
                } :: m
            (q, models)
        | None, _ -> (p, m)


    let tryCreateModelWithBase picker creator baseGetter baseCreator (p, m) =
        let create b d u =
            {
                model = creator b d
                usage = u
            }

        match tryPickParam picker p with
        | Some (u, d), q ->
            match tryGetModel baseGetter m with
            | Some b -> q, (create b d u) :: m
            | None ->
                let (q1, m1) = baseCreator (q, m)

                match tryGetModel baseGetter m1 with
                | Some b -> q1, (create b d u) :: m1
                | None -> (q1, m1)
        | None, _ -> (p, m)


    type FoodCreationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | FoodCreationRateModel d -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            tryCreateModel FoodCreationParam.paramGetter (fun d -> d |> FoodCreationModel |> FoodCreationRateModel) (p, m)


    type WasteRemovalModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | WasteRemovalRateModel d -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            tryCreateModel WasteRemovalParam.paramGetter (fun d -> d |> WasteRemovalModel |> WasteRemovalRateModel) (p, m)


    type WasteRecyclingModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | WasteRecyclingRateModel d -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            tryCreateModel WasteRecyclingParam.paramGetter (fun d -> d |> WasteRecyclingModel |> WasteRecyclingRateModel) (p, m)


    type SedimentationDirectRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SedimentationDirectRateModel (SedDirRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            tryCreateModel SedimentationDirectRandomParam.paramGetter (fun d -> d |> SedimentationDirectRandomModel |> SedDirRndModel |> SedimentationDirectRateModel) (p, m)


    type SedimentationDirectSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | SedimentationDirectRateParam (SedDirSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SedimentationDirectRateModel (SedDirSimModel d) -> Some d
            | _ -> None


        static member tryCreate (a, r) (p, m) =
            let creator b (d : SedimentationDirectSimilarParam) = { sedDirModel = b; aminoAcids = a; sedDirSimParam = d.sedDirSimParam; reagents = r } |> SedimentationDirectSimilarModel |> SedDirSimModel |> SedimentationDirectRateModel
            tryCreateModelWithBase SedimentationDirectSimilarModel.paramGetter creator SedimentationDirectRandomModel.modelGetter SedimentationDirectRandomModel.tryCreate (p, m)


    type SedimentationDirectModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SedimentationDirectRateModel d -> Some d
            | _ -> None

        static member tryCreate (p, m) = (p, m) |> SedimentationDirectRandomModel.tryCreate


    type SedimentationAllRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SedimentationAllRateModel d -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            tryCreateModel SedimentationAllRandomParam.paramGetter (fun d -> d |> SedimentationAllRandomModel |> SedAllRndModel |> SedimentationAllRateModel) (p, m)


    type SedimentationAllModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SedimentationAllRateModel d -> Some d
            | _ -> None

        static member tryCreate (p, m) = (p, m) |> SedimentationAllRandomModel.tryCreate


    type SynthesisRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SynthesisRateModel (SynthRndModel d) -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            tryCreateModel SynthesisRandomParam.paramGetter (fun d -> d |> SynthesisRandomModel |> SynthRndModel |> SynthesisRateModel) (p, m)


    type SynthesisModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SynthesisRateModel d -> Some d
            | _ -> None


        static member tryCreate (p, m) = (p, m) |> SynthesisRandomModel.tryCreate


    type CatalyticSynthesisRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticSynthesisRateModel (CatSynthRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d = { synthesisModel = b; catSynthRndParam = d } |> CatalyticSynthesisRandomModel |> CatSynthRndModel |> CatalyticSynthesisRateModel
            tryCreateModelWithBase CatalyticSynthesisRandomParam.paramGetter creator SynthesisModel.modelGetter SynthesisModel.tryCreate (p, m)


    type CatalyticSynthesisSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | CatalyticSynthesisRateParam (CatSynthSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticSynthesisRateModel (CatSynthSimModel d) -> Some d
            | _ -> None


        static member tryCreate a (p, m) =
            let creator b (d : CatalyticSynthesisSimilarParam) = { catSynthModel = b; aminoAcids = a; catSynthSimParam = d.catSynthSimParam } |> CatalyticSynthesisSimilarModel |> CatSynthSimModel |> CatalyticSynthesisRateModel
            tryCreateModelWithBase CatalyticSynthesisSimilarModel.paramGetter creator CatalyticSynthesisRandomModel.modelGetter CatalyticSynthesisRandomModel.tryCreate (p, m)


    type CatalyticSynthesisModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticSynthesisRateModel d -> Some d
            | _ -> None

        static member tryCreate a (p, m) =
            (p, m)
            |> CatalyticSynthesisRandomModel.tryCreate
            |> CatalyticSynthesisSimilarModel.tryCreate a


    type DestructionModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | DestructionRateModel d -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            tryCreateModel DestructionRandomParam.paramGetter (fun d -> d |> DestructionRandomModel |> DestrRndModel |> DestructionRateModel) (p, m)


    type CatalyticDestructionRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticDestructionRateModel (CatDestrRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d = { destructionModel = b; catDestrRndParam = d } |> CatalyticDestructionRandomModel |> CatDestrRndModel |> CatalyticDestructionRateModel
            tryCreateModelWithBase CatalyticDestructionRandomParam.paramGetter creator DestructionModel.modelGetter DestructionModel.tryCreate (p, m)


    type CatalyticDestructionSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | CatalyticDestructionRateParam (CatDestrSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticDestructionRateModel (CatDestrSimModel d) -> Some d
            | _ -> None


        static member tryCreate a (p, m) =
            let creator b (d : CatalyticDestructionSimilarParam) = 
                { catDestrModel = b; aminoAcids = a; catDestrSimParam = d.catDestrSimParam } |> CatalyticDestructionSimilarModel |> CatDestrSimModel |> CatalyticDestructionRateModel

            tryCreateModelWithBase CatalyticDestructionSimilarModel.paramGetter creator CatalyticDestructionRandomModel.modelGetter CatalyticDestructionRandomModel.tryCreate (p, m)


    type CatalyticDestructionModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticDestructionRateModel d -> Some d
            | _ -> None

        static member tryCreate a (p, m) =
            (p, m)
            |> CatalyticDestructionRandomModel.tryCreate
            |> CatalyticDestructionSimilarModel.tryCreate a


    type LigationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | LigationRateModel (LigRndModel d) -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            tryCreateModel LigationRandomParam.paramGetter (fun d -> d |> LigationRandomModel |> LigRndModel |> LigationRateModel) (p, m)


    type LigationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | LigationRateModel d -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            (p, m)
            |> LigationRandomModel.tryCreate


    type CatalyticLigationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticLigationRateModel (CatLigRndModel d) -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            let creator b d = { ligationModel = b; catLigationParam = d } |> CatalyticLigationRandomModel |> CatLigRndModel |> CatalyticLigationRateModel
            tryCreateModelWithBase CatalyticLigationRandomParam.paramGetter creator LigationModel.modelGetter LigationModel.tryCreate (p, m)


    type CatalyticLigationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticLigationRateModel d -> Some d
            | _ -> None

        static member tryCreate (p, m) =
            (p, m)
            |> CatalyticLigationRandomModel.tryCreate


    type RacemizationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | RacemizationRateModel d -> Some d
            | _ -> None


        static member tryCreate (p, m) =
            tryCreateModel RacemizationRandomParam.paramGetter (fun d -> d |> RacemizationRandomModel |> RacemRndModel |> RacemizationRateModel) (p, m)


    type CatalyticRacemizationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticRacemizationRateModel (CatRacemRndModel d) -> Some d
            | _ -> None

        static member tryCreate a (p, m) =
            let creator b d = { racemizationModel = b; aminoAcids = a; catRacemRndParam = d } |> CatalyticRacemizationRandomModel |> CatRacemRndModel |> CatalyticRacemizationRateModel
            tryCreateModelWithBase CatalyticRacemizationRandomParam.paramGetter creator RacemizationModel.modelGetter RacemizationModel.tryCreate (p, m)


    type CatalyticRacemizationSimilarModel
        with

        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | CatalyticRacemizationRateParam (CatRacemSimParam d) -> Some (p.usage, d)
            | _ -> None

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticRacemizationRateModel (CatRacemSimModel d) -> Some d
            | _ -> None


        static member tryCreate a (p, m) =
            let creator b (d : CatalyticRacemizationSimilarParam) =
                { catRacemModel = b; aminoAcids = a; catRacemSimParam = d.catRacemSimParam } |> CatalyticRacemizationSimilarModel |> CatRacemSimModel |> CatalyticRacemizationRateModel

            tryCreateModelWithBase CatalyticRacemizationSimilarModel.paramGetter creator CatalyticRacemizationRandomModel.modelGetter (CatalyticRacemizationRandomModel.tryCreate a) (p, m)


    type CatalyticRacemizationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticRacemizationRateModel d -> Some d
            | _ -> None

        static member tryCreate a (p, m) =
            (p, m)
            |> CatalyticRacemizationRandomModel.tryCreate a
            |> CatalyticRacemizationSimilarModel.tryCreate a


    type ReactionRateModel
        with

        static member createAll (p : list<ReactionRateModelParamWithUsage>) (n : NumberOfAminoAcids) =
            let a = AminoAcid.getAminoAcids n

            (
                [
                    FoodCreationModel.tryCreate
                    WasteRemovalModel.tryCreate
                    WasteRecyclingModel.tryCreate
                    SynthesisModel.tryCreate
                    DestructionModel.tryCreate
                    CatalyticSynthesisModel.tryCreate a
                    CatalyticDestructionModel.tryCreate a
                    LigationModel.tryCreate
                    CatalyticLigationModel.tryCreate
                    SedimentationDirectModel.tryCreate
                    SedimentationAllModel.tryCreate
                    RacemizationModel.tryCreate
                    CatalyticRacemizationModel.tryCreate a
                ]
                |> List.fold (fun acc r -> r acc) (p, [])
            )
            |> snd
