namespace ClmImpure.ReactionRateModelExtensions

open ClmImpure.ReactionRateModels
open ClmImpure.ReactionRateModels.SugarSynthesisModel

module SugarSynthesisModelExt =

    type SugarSynthesisModel
        with

        static member modelGetter (p : ReactionRateModelWithUsage) =
            match p.model with
            | SugarSynthesisRateModel d -> Some d
            | _ -> None


        static member tryCreate (p, m) = (p, m) |> SynthesisRandomModel.tryCreate
