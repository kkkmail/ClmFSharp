namespace ClmImpure.ReactionRateModelExtensions

open ClmImpure.ReactionRateModelsAll
open ClmImpure.ReactionRateModels.SynthesisRandomModel
open ClmImpure.ReactionRateModels.SugarSynthesisModel
open ClmImpure.ReactionRateModelExtensions.SynthesisRandomModelExt

module SugarSynthesisModelExt =

    type SugarSynthesisModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SugarSynthesisRateModel d -> Some d
            | _ -> None


        static member tryCreate (p, m) = (p, m) |> SynthesisRandomModel.tryCreate
