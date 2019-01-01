namespace ContGen

open Clm.Substances
open Clm.Distributions
open Clm.ReactionRates
open DatabaseTypes

module RateModelsExt = 

    //type ReactionRateModelParam =
    //    | FoodCreationRateParam of FoodCreationParam
    //    | WasteRemovalRateParam of WasteRemovalParam
    //    | WasteRecyclingRateParam of WasteRecyclingParam
    //    | SedimentationDirectRateParam of SedimentationDirectParam
    //    | SedimentationAllRateParam of SedimentationAllParam

    //type ReactionRateModel =
    //    | FoodCreationRateModel of FoodCreationModel
    //    | WasteRemovalRateModel of WasteRemovalModel
    //    | WasteRecyclingRateModel of WasteRecyclingModel
    //    | SedimentationDirectRateModel of SedimentationDirectModel
    //    | SedimentationAllRateModel of SedimentationAllModel


    type ModelsAndParams = 
        {
            models : list<ReactionRateModelWithUsage>
            modelParams : list<ReactionRateModelParamWithUsage>
            aminoAcids : list<AminoAcid>
        }


    let tryPickParam picker (mp : ModelsAndParams) =
        let rec inner a b =
            match a with
            | [] -> None, b |> List.rev
            | h :: t ->
                match picker h with
                | Some x -> Some x, (b |> List.rev) @ t
                | None -> inner t (h :: b)

        let (x, y) = inner mp.modelParams []
        (x, { mp with modelParams = y })


    let tryGetModel getter (p : list<ReactionRateModelWithUsage>) = p |> List.tryPick getter


    type SynthesisRandomParam
        with
        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | SynthesisRateParam (SynthRndParam d) -> Some (p.usage, d)
            | _ -> None


    type SynthesisModel
        with
        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SynthesisRateModel d -> Some d
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


    type CatalyticSynthesisRandomParam
        with
        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | CatalyticSynthesisRateParam (CatSynthRndParam d) -> Some (p.usage, d)
            | _ -> None


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


    type DestructionRandomParam
        with
        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | DestructionRateParam (DestrRndParam d) -> Some (p.usage, d)
            | _ -> None


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


    type CatalyticDestructionRandomParam
        with
        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | CatalyticDestructionRateParam (CatDestrRndParam d) -> Some (p.usage, d)
            | _ -> None


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


    type LigationRandomParam
        with
        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | LigationRateParam (LigRndParam d) -> Some (p.usage, d)
            | _ -> None


    type LigationModel
        with
        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | LigationRateModel d -> Some d
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


    type CatalyticLigationRandomParam
        with
        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | CatalyticLigationRateParam (CatLigRndParam d) -> Some (p.usage, d)
            | _ -> None


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


    type RacemizationRandomParam
        with
        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | RacemizationRateParam (RacemRndParam d) -> Some (p.usage, d)
            | _ -> None


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


    type CatalyticRacemizationRandomParam
        with
        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | CatalyticRacemizationRateParam (CatRacemRndParam d) -> Some (p.usage, d)
            | _ -> None


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

