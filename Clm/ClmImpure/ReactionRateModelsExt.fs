namespace ClmImpure

open Clm.Substances
open Clm.ReactionRates

open ClmImpure.ReactionRateFunctions
open ClmImpure.ReactionRateModels

module ReactionRateModelsExt =

    type ModelsAndParams =
        {
            models : list<ReactionRateModelWithUsage>
            reactionRateParams : ReactionRateParams
        }


    let tryGetModel getter (p : list<ReactionRateModelWithUsage>) = p |> List.tryPick getter


    type FoodCreationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | FoodCreationRateModel d -> Some d
            | _ -> None

        static member tryCreate (mp : ModelsAndParams) =
            match tryPickParam FoodCreationParam.paramGetter mp with
            | Some (u, d), q ->
                {
                    models =
                        {
                            model =
                                d
                                |> FoodCreationModel
                                |> FoodCreationRateModel
                            usage = u
                        } :: q.models
                    modelParams = q.modelParams
                    aminoAcids = q.aminoAcids
                }
            | None, _ -> mp


    type WasteRemovalModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | WasteRemovalRateModel d -> Some d
            | _ -> None


        static member tryCreate (mp : ModelsAndParams) =
            match tryPickParam WasteRemovalParam.paramGetter mp with
            | Some (u, d), q ->
                {
                    models =
                        {
                            model =
                                d
                                |> WasteRemovalModel
                                |> WasteRemovalRateModel
                            usage = u
                        } :: q.models
                    modelParams = q.modelParams
                    aminoAcids = q.aminoAcids
                }
            | None, _ -> mp


    type WasteRecyclingModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | WasteRecyclingRateModel d -> Some d
            | _ -> None


        static member tryCreate (mp : ModelsAndParams) =
            match tryPickParam WasteRecyclingParam.paramGetter mp with
            | Some (u, d), q ->
                {
                    models =
                        {
                            model =
                                d
                                |> WasteRecyclingModel
                                |> WasteRecyclingRateModel
                            usage = u
                        } :: q.models
                    modelParams = q.modelParams
                    aminoAcids = q.aminoAcids
                }
            | None, _ -> mp


    type SedimentationDirectRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SedimentationDirectRateModel d -> Some d
            | _ -> None


        static member tryCreate (mp : ModelsAndParams) =
            match tryPickParam SedimentationDirectRandomParam.paramGetter mp with
            | Some (u, d), q ->
                {
                    models =
                        {
                            model =
                                d
                                |> SedimentationDirectRandomModel
                                |> SedDirRndModel
                                |> SedimentationDirectRateModel
                            usage = u
                        } :: q.models
                    modelParams = q.modelParams
                    aminoAcids = q.aminoAcids
                }
            | None, _ -> mp


    type SedimentationDirectModel
        with

        static member tryCreate (mp : ModelsAndParams) =
            mp
            |> SedimentationDirectRandomModel.tryCreate


    type SedimentationAllRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SedimentationAllRateModel d -> Some d
            | _ -> None


        static member tryCreate (mp : ModelsAndParams) =
            match tryPickParam SedimentationAllRandomParam.paramGetter mp with
            | Some (u, d), q ->
                {
                    models =
                        {
                            model =
                                d
                                |> SedimentationAllRandomModel
                                |> SedAllRndModel
                                |> SedimentationAllRateModel
                            usage = u
                        } :: q.models
                    modelParams = q.modelParams
                    aminoAcids = q.aminoAcids
                }
            | None, _ -> mp


    type SedimentationAllModel
        with

        static member tryCreate (mp : ModelsAndParams) =
            mp
            |> SedimentationAllRandomModel.tryCreate


    type SynthesisRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SynthesisRateModel (SynthRndModel d) -> Some d
            | _ -> None


        static member tryCreate (mp : ModelsAndParams) =
            match tryPickParam SynthesisRandomParam.paramGetter mp with
            | Some (u, d), q ->
                {
                    models =
                        {
                            model =
                                d
                                |> SynthesisRandomModel
                                |> SynthRndModel
                                |> SynthesisRateModel
                            usage = u
                        } :: q.models
                    modelParams = q.modelParams
                    aminoAcids = q.aminoAcids
                }
            | None, _ -> mp


    type SynthesisModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SynthesisRateModel d -> Some d
            | _ -> None


        static member tryCreate (mp : ModelsAndParams) =
            mp
            |> SynthesisRandomModel.tryCreate


    type CatalyticSynthesisRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticSynthesisRateModel (CatSynthRndModel d) -> Some d
            | _ -> None

        static member tryCreate (mp : ModelsAndParams) =
            let create d m u q =
                {
                    models =
                        {
                            model =
                                {
                                    catSynthRndParam = d
                                    synthesisModel = m
                                }
                                |> CatalyticSynthesisRandomModel
                                |> CatSynthRndModel
                                |> CatalyticSynthesisRateModel
                            usage = u
                        } :: q.models
                    modelParams = q.modelParams
                    aminoAcids = q.aminoAcids
                }

            match tryPickParam CatalyticSynthesisRandomParam.paramGetter mp with
            | Some (u, d), q ->
                match tryGetModel SynthesisModel.modelGetter mp.models with
                | Some m -> create d m u q
                | None ->
                    let x = SynthesisModel.tryCreate mp

                    match tryGetModel SynthesisModel.modelGetter x.models with
                    | Some m -> create d m u x
                    | None -> x
            | None, _ -> mp


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


        static member tryCreate (mp : ModelsAndParams) =
            let create d m u (q : ModelsAndParams) =
                {
                    models =
                        {
                            model =
                                {
                                    catSynthModel = m
                                    aminoAcids = q.aminoAcids
                                    catSynthSimParam = d
                                }
                                |> CatalyticSynthesisSimilarModel
                                |> CatSynthSimModel
                                |> CatalyticSynthesisRateModel
                            usage = u
                        } :: q.models
                    modelParams = q.modelParams
                    aminoAcids = q.aminoAcids
                }

            match tryPickParam CatalyticSynthesisSimilarModel.paramGetter mp with
            | Some (u, d), q ->
                match tryGetModel CatalyticSynthesisRandomModel.modelGetter mp.models with
                | Some m -> create d m u q
                | None ->
                    let x = CatalyticSynthesisRandomModel.tryCreate mp

                    match tryGetModel CatalyticSynthesisRandomModel.modelGetter x.models with
                    | Some m -> create d m u x
                    | None -> x
            | None, _ -> mp


    type CatalyticSynthesisModel
        with

        static member tryCreate (mp : ModelsAndParams) =
            mp
            |> CatalyticSynthesisRandomModel.tryCreate
            |> CatalyticSynthesisSimilarModel.tryCreate


    type DestructionModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | DestructionRateModel d -> Some d
            | _ -> None


        static member tryCreate (mp : ModelsAndParams) =
            match tryPickParam DestructionRandomParam.paramGetter mp with
            | Some (u, d), q ->
                {
                    models =
                        {
                            model =
                                d
                                |> DestructionRandomModel
                                |> DestrRndModel
                                |> DestructionRateModel
                            usage = u
                        } :: q.models
                    modelParams = q.modelParams
                    aminoAcids = q.aminoAcids
                }
            | None, _ -> mp


    type CatalyticDestructionRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticDestructionRateModel (CatDestrRndModel d) -> Some d
            | _ -> None

        static member tryCreate (mp : ModelsAndParams) =
            let create d m u q =
                {
                    models =
                        {
                            model =
                                {
                                    catDestrRndParam = d
                                    destructionModel = m
                                }
                                |> CatalyticDestructionRandomModel
                                |> CatDestrRndModel
                                |> CatalyticDestructionRateModel
                            usage = u
                        } :: q.models
                    modelParams = q.modelParams
                    aminoAcids = q.aminoAcids
                }

            match tryPickParam CatalyticDestructionRandomParam.paramGetter mp with
            | Some (u, d), q ->
                match tryGetModel DestructionModel.modelGetter mp.models with
                | Some m -> create d m u q
                | None ->
                    let x = DestructionModel.tryCreate mp

                    match tryGetModel DestructionModel.modelGetter x.models with
                    | Some m -> create d m u x
                    | None -> x
            | None, _ -> mp


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


        static member tryCreate (mp : ModelsAndParams) =
            let create d m u (q : ModelsAndParams) =
                {
                    models =
                        {
                            model =
                                {
                                    catDestrModel = m
                                    aminoAcids = q.aminoAcids
                                    catDestrSimParam = d
                                }
                                |> CatalyticDestructionSimilarModel
                                |> CatDestrSimModel
                                |> CatalyticDestructionRateModel
                            usage = u
                        } :: q.models
                    modelParams = q.modelParams
                    aminoAcids = q.aminoAcids
                }

            match tryPickParam CatalyticDestructionSimilarModel.paramGetter mp with
            | Some (u, d), q ->
                match tryGetModel CatalyticDestructionRandomModel.modelGetter mp.models with
                | Some m -> create d m u q
                | None ->
                    let x = CatalyticDestructionRandomModel.tryCreate mp

                    match tryGetModel CatalyticDestructionRandomModel.modelGetter x.models with
                    | Some m -> create d m u x
                    | None -> x
            | None, _ -> mp


    type CatalyticDestructionModel
        with

        static member tryCreate (mp : ModelsAndParams) =
            mp
            |> CatalyticDestructionRandomModel.tryCreate
            |> CatalyticDestructionSimilarModel.tryCreate


    type LigationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | LigationRateModel (LigRndModel d) -> Some d
            | _ -> None


        static member tryCreate (mp : ModelsAndParams) =
            match tryPickParam LigationRandomParam.paramGetter mp with
            | Some (u, d), q ->
                {
                    models =
                        {
                            model =
                                d
                                |> LigationRandomModel
                                |> LigRndModel
                                |> LigationRateModel
                            usage = u
                        } :: q.models
                    modelParams = q.modelParams
                    aminoAcids = q.aminoAcids
                }
            | None, _ -> mp


    type LigationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | LigationRateModel d -> Some d
            | _ -> None


        static member tryCreate (mp : ModelsAndParams) =
            mp
            |> LigationRandomModel.tryCreate


    type CatalyticLigationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticLigationRateModel (CatLigRndModel d) -> Some d
            | _ -> None

        static member tryCreate (mp : ModelsAndParams) =
            let create d m u q =
                {
                    models =
                        {
                            model =
                                {
                                    catLigationParam = d
                                    ligationModel = m
                                }
                                |> CatalyticLigationRandomModel
                                |> CatLigRndModel
                                |> CatalyticLigationRateModel
                            usage = u
                        } :: q.models
                    modelParams = q.modelParams
                    aminoAcids = q.aminoAcids
                }

            match tryPickParam CatalyticLigationRandomParam.paramGetter mp with
            | Some (u, d), q ->
                match tryGetModel LigationModel.modelGetter mp.models with
                | Some m -> create d m u q
                | None ->
                    let x = LigationModel.tryCreate mp

                    match tryGetModel LigationModel.modelGetter x.models with
                    | Some m -> create d m u x
                    | None -> x
            | None, _ -> mp


    type CatalyticLigationModel
        with

        static member tryCreate (mp : ModelsAndParams) =
            mp
            |> CatalyticLigationRandomModel.tryCreate


    type RacemizationModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | RacemizationRateModel d -> Some d
            | _ -> None


        static member tryCreate (mp : ModelsAndParams) =
            match tryPickParam RacemizationRandomParam.paramGetter mp with
            | Some (u, d), q ->
                {
                    models =
                        {
                            model =
                                d
                                |> RacemizationRandomModel
                                |> RacemRndModel
                                |> RacemizationRateModel
                            usage = u
                        } :: q.models
                    modelParams = q.modelParams
                    aminoAcids = q.aminoAcids
                }
            | None, _ -> mp


    type CatalyticRacemizationRandomModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | CatalyticRacemizationRateModel (CatRacemRndModel d) -> Some d
            | _ -> None

        static member tryCreate (mp : ModelsAndParams) =
            let create d m u q =
                {
                    models =
                        {
                            model =
                                {
                                    catRacemRndParam = d
                                    racemizationModel = m
                                    aminoAcids = q.aminoAcids
                                }
                                |> CatalyticRacemizationRandomModel
                                |> CatRacemRndModel
                                |> CatalyticRacemizationRateModel
                            usage = u
                        } :: q.models
                    modelParams = q.modelParams
                    aminoAcids = q.aminoAcids
                }

            match tryPickParam CatalyticRacemizationRandomParam.paramGetter mp with
            | Some (u, d), q ->
                match tryGetModel RacemizationModel.modelGetter mp.models with
                | Some m -> create d m u q
                | None ->
                    let x = RacemizationModel.tryCreate mp

                    match tryGetModel RacemizationModel.modelGetter x.models with
                    | Some m -> create d m u x
                    | None -> x
            | None, _ -> mp


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


        static member tryCreate (mp : ModelsAndParams) =
            let create d m u (q : ModelsAndParams) =
                {
                    models =
                        {
                            model =
                                {
                                    catRacemModel = m
                                    aminoAcids = q.aminoAcids
                                    catRacemSimParam = d
                                }
                                |> CatalyticRacemizationSimilarModel
                                |> CatRacemSimModel
                                |> CatalyticRacemizationRateModel
                            usage = u
                        } :: q.models
                    modelParams = q.modelParams
                    aminoAcids = q.aminoAcids
                }

            match tryPickParam CatalyticRacemizationSimilarModel.paramGetter mp with
            | Some (u, d), q ->
                match tryGetModel CatalyticRacemizationRandomModel.modelGetter mp.models with
                | Some m -> create d m u q
                | None ->
                    let x = CatalyticRacemizationRandomModel.tryCreate mp

                    match tryGetModel CatalyticRacemizationRandomModel.modelGetter x.models with
                    | Some m -> create d m u x
                    | None -> x
            | None, _ -> mp


    type CatalyticRacemizationModel
        with

        static member tryCreate (mp : ModelsAndParams) =
            mp
            |> CatalyticRacemizationRandomModel.tryCreate
            |> CatalyticRacemizationSimilarModel.tryCreate


    type ReactionRateModel
        with

        static member createAll (p : list<ReactionRateModelParamWithUsage>) (n : NumberOfAminoAcids) =
            let mp =
                {
                    models = []
                    modelParams = p
                    aminoAcids= AminoAcid.getAminoAcids n
                }
            (
                [
                    FoodCreationModel.tryCreate
                    WasteRemovalModel.tryCreate
                    WasteRecyclingModel.tryCreate
                    SynthesisModel.tryCreate
                    DestructionModel.tryCreate
                    CatalyticSynthesisModel.tryCreate
                    CatalyticDestructionModel.tryCreate
                    LigationModel.tryCreate
                    CatalyticLigationModel.tryCreate
                    SedimentationDirectModel.tryCreate
                    SedimentationAllModel.tryCreate
                    RacemizationModel.tryCreate
                    CatalyticRacemizationModel.tryCreate
                ]
                |> List.fold (fun acc r -> r acc) mp
            ).models
