namespace ContGen

open Clm.Distributions
open Clm.ReactionRates
open DatabaseTypes

module RateModelsExt = 

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

    //type ReactionRateModel =
    //    | FoodCreationRateModel of FoodCreationModel
    //    | WasteRemovalRateModel of WasteRemovalModel
    //    | WasteRecyclingRateModel of WasteRecyclingModel
    //    | SynthesisRateModel of SynthesisModel
    //    | DestructionRateModel of DestructionModel
    //    |  of 
    //    | CatalyticDestructionRateModel of CatalyticDestructionModel
    //    | LigationRateModel of LigationModel
    //    | CatalyticLigationRateModel of CatalyticLigationModel
    //    | SedimentationDirectRateModel of SedimentationDirectModel
    //    | SedimentationAllRateModel of SedimentationAllModel
    //    | RacemizationRateModel of RacemizationModel
    //    | CatalyticRacemizationRateModel of CatalyticRacemizationModel


    type ModelsAndParams = 
        {
            models : list<ReactionRateModelWithUsage>
            modelParams : list<ReactionRateModelParamWithUsage>
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
        x, { mp with modelParams = y }


    let tryGetModel getter (p : list<ReactionRateModelWithUsage>) =
            p |> List.tryPick getter


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
                    models = { model = d |> SynthesisRandomModel |> SynthRndModel |> SynthesisRateModel; usage = u } :: q.models
                    modelParams = q.modelParams
                }
            | None, _ -> mp


    type CatalyticSynthesisRandomParam
        with
        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | CatalyticSynthesisRateParam (CatSynthRndParam d) -> Some (p.usage, d)
            | _ -> None


    type CatRatesSimilarityParam
        with
        static member paramGetter (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | CatalyticSynthesisRateParam (CatSynthSimParam d) -> Some (p.usage, d)
            | _ -> None



    //type CatalyticSynthesisModel =
    //    | CatSynthRndModel of CatalyticSynthesisRandomModel
    //    | CatSynthSimModel of CatalyticSynthesisSimilarModel

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


    //type CatalyticSynthesisModel
    //    with
    //    static member modelGetter (p : ReactionRateModelWithUsage) =
    //        match p.model with
    //        | CatalyticSynthesisRateModel d -> Some d
    //        | _ -> None


    //    static member tryCreate (mp : ModelsAndParams) =
    //        match tryPickParam CatalyticSynthesisRandomParam.paramGetter mp.modelParams with
    //        | Some (u, d), q ->
    //            let create d m u =
    //                {
    //                    model =
    //                        {
    //                            catSynthRndParam = d
    //                            synthesisModel = m
    //                        }
    //                        |> CatalyticSynthesisRandomModel
    //                        |> CatSynthRndModel
    //                        |> CatalyticSynthesisRateModel
    //                    usage = u
    //                }

    //            match tryPickModel SynthesisModel.modelGetter mp.models with
    //            | Some m ->
    //                {
    //                    models = create d m u :: mp.models
    //                    modelParams = q
    //                }
    //            | None ->
    //                let x = SynthesisModel.tryCreate mp

    //                //{ model = d |> CatalyticSynthesisRandomModel |> SynthRndModel |> SynthesisRateModel; usage = u } |> Some, x
    //                failwith ""
    //        | None, _ -> 
    //            mp
