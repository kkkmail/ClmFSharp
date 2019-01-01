namespace ContGen

open Clm.Distributions
open Clm.ReactionRates
open DatabaseTypes

module RateModelsExt = 

    //type ReactionRateModel =
    //    | FoodCreationRateModel of FoodCreationModel
    //    | WasteRemovalRateModel of WasteRemovalModel
    //    | WasteRecyclingRateModel of WasteRecyclingModel
    //    | SynthesisRateModel of SynthesisModel
    //    | DestructionRateModel of DestructionModel
    //    | CatalyticSynthesisRateModel of CatalyticSynthesisModel
    //    | CatalyticDestructionRateModel of CatalyticDestructionModel
    //    | LigationRateModel of LigationModel
    //    | CatalyticLigationRateModel of CatalyticLigationModel
    //    | SedimentationDirectRateModel of SedimentationDirectModel
    //    | SedimentationAllRateModel of SedimentationAllModel
    //    | RacemizationRateModel of RacemizationModel
    //    | CatalyticRacemizationRateModel of CatalyticRacemizationModel

    let tryGetParam (p : list<ReactionRateModelParamWithUsage>) getter = 
        p |> List.map getter |> List.choose id |> List.sort |> List.tryHead

    type SynthesisModel
        with
        static member tryToSynthRndParam (p : ReactionRateModelParamWithUsage) =
            match p.modelParam with
            | SynthesisRateParam (SynthRndParam d) -> Some (p.usage, d)
            | _ -> None

        static member tryCreate (r : list<ReactionRateModelWithUsage>) (p : list<ReactionRateModelParamWithUsage>) =
            let x = 
                match tryGetParam p SynthesisModel.tryToSynthRndParam with
                | Some (u, d) -> 0
                | None -> 0
            0

