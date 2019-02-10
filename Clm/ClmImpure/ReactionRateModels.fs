namespace ClmImpure

open System
open System.Collections.Generic
open FSharp.Collections

open Clm.Substances
open Clm.Distributions
open Clm.ReactionTypes
open Clm.ReactionRates

open ClmImpure.ReactionRateFunctions

module ReactionRateModels =

    [<AbstractClass>]
    type RateModel<'P, 'R when 'R : equality> (p : 'P) =
        let rateDictionaryImpl = new Dictionary<'R, RateData>()
        member __.rateDictionary = rateDictionaryImpl
        member __.inputParams = p

        member __.getAllRates() = getAllRatesImpl rateDictionaryImpl


    type FoodCreationModel (p : FoodCreationParam) =
        inherit RateModel<FoodCreationParam, FoodCreationReaction>(p)
        let calculateRates _ = getRates (Some p.foodCreationRate, Some 1.0) (None, None)
        member model.getRates r = getRatesImpl model.rateDictionary getEnantiomer calculateRates r


    type WasteRemovalModel (p : WasteRemovalParam) =
        inherit RateModel<WasteRemovalParam, WasteRemovalReaction>(p)
        let calculateRates _ = getRates (Some p.wasteRemovalRate, Some 1.0) (None, None)
        member model.getRates r = getRatesImpl model.rateDictionary getEnantiomer calculateRates r


    type WasteRecyclingModel (p : WasteRecyclingParam) =
        inherit RateModel<WasteRecyclingParam, WasteRecyclingReaction>(p)
        let calculateRates _ = getRates (Some p.wasteRecyclingRate, Some 1.0) (None, None)
        member model.getRates r = getRatesImpl model.rateDictionary getEnantiomer calculateRates r


    type SynthesisRandomModel (p : SynthesisRandomParam) =
        inherit RateModel<SynthesisRandomParam, SynthesisReaction>(p)

        let calculateRates rnd _ =
            let d = p.synthesisDistribution
            getRates (p.forwardScale, d.nextDouble rnd |> Some) (p.backwardScale, d.nextDouble rnd |> Some)

        member model.getRates rnd r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd) r


    type SynthesisModel =
        | SynthRndModel of SynthesisRandomModel

        member model.getRates rnd r =
            match model with
            | SynthRndModel m -> m.getRates rnd r

        member model.inputParams =
            match model with
            | SynthRndModel m -> m.inputParams |> SynthRndParam

        member model.getAllRates() =
            match model with
            | SynthRndModel m -> m.getAllRates()

        static member create p =
            match p with
            | SynthRndParam q -> SynthesisRandomModel q |> SynthRndModel


    type CatalyticSynthesisRandomParamWithModel =
        {
            catSynthRndParam : CatalyticSynthesisRandomParam
            synthesisModel : SynthesisModel
        }


    type CatalyticSynthesisRandomModel (p : CatalyticSynthesisRandomParamWithModel) =
        inherit RateModel<CatalyticSynthesisRandomParamWithModel, CatalyticSynthesisReaction>(p)

        let calculateCatSynthRates rnd t (CatalyticSynthesisReaction (s, c)) =
            {
                reaction = s
                catalyst = c
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticSynthesisReaction
                getBaseRates = p.synthesisModel.getRates rnd
                eeParams = p.catSynthRndParam.catSynthRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateCatSynthRates rnd t) r


    type CatalyticSynthesisSimilarParamWithModel =
        {
            catSynthModel : CatalyticSynthesisRandomModel
            aminoAcids : list<AminoAcid>
            catSynthSimParam : CatRatesSimilarityParam
        }


    type CatalyticSynthesisParamWithModel =
        | CatSynthRndParamWithModel of CatalyticSynthesisRandomParamWithModel
        | CatSynthSimParamWithModel of CatalyticSynthesisSimilarParamWithModel


    type CatalyticSynthesisSimilarModel (p : CatalyticSynthesisSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (CatalyticSynthesisReaction (s, c)) =
            let (SynthesisReaction a) = s
            {
                reaction = s
                catalyst = c
                aminoAcids = p.aminoAcids
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticSynthesisReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> a.createSameChirality e |> SynthesisReaction)
                getBaseRates = p.catSynthModel.inputParams.synthesisModel.getRates rnd
                getBaseCatRates = p.catSynthModel.getRates rnd t
                simParams = p.catSynthSimParam
                eeParams = p.catSynthModel.inputParams.catSynthRndParam.catSynthRndEeParams
                rateDictionary = p.catSynthModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateSimRates

        member __.getRates rnd t r = calculateSimRatesImpl rnd t r
        member __.inputParams = p
        member __.getAllRates() = getAllRatesImpl p.catSynthModel.rateDictionary


    type CatalyticSynthesisModel =
        | CatSynthRndModel of CatalyticSynthesisRandomModel
        | CatSynthSimModel of CatalyticSynthesisSimilarModel

        member model.getRates rnd t r =
            match model with
            | CatSynthRndModel m -> m.getRates rnd t r
            | CatSynthSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | CatSynthRndModel m -> m.inputParams |> CatSynthRndParamWithModel
            | CatSynthSimModel m -> m.inputParams |> CatSynthSimParamWithModel

        member model.getAllRates() =
            match model with
            | CatSynthRndModel m -> m.getAllRates()
            | CatSynthSimModel m -> m.getAllRates()

        static member create p =
            match p with 
            | CatSynthRndParamWithModel q -> CatalyticSynthesisRandomModel q |> CatSynthRndModel
            | CatSynthSimParamWithModel q -> CatalyticSynthesisSimilarModel q |> CatSynthSimModel


    type DestructionRandomModel (p : DestructionRandomParam) =
        inherit RateModel<DestructionRandomParam, DestructionReaction>(p)

        let calculateRates rnd _ =
            let d = p.destructionDistribution
            getRates (p.forwardScale, d.nextDouble rnd |> Some) (p.backwardScale, d.nextDouble rnd |> Some)

        member model.getRates rnd r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd) r


    type DestructionModel =
        | DestrRndModel of DestructionRandomModel

        member model.getRates rnd r =
            match model with
            | DestrRndModel m -> m.getRates rnd r

        member model.inputParams =
            match model with
            | DestrRndModel m -> m.inputParams |> DestrRndParam

        member model.getAllRates() =
            match model with
            | DestrRndModel m -> m.getAllRates()

        static member create p =
            match p with
            | DestrRndParam q -> DestructionRandomModel q |> DestrRndModel


    type CatalyticDestructionRandomParamWithModel =
        {
            catDestrRndParam : CatalyticDestructionRandomParam
            destructionModel : DestructionModel
        }


    type CatalyticDestructionRandomModel (p : CatalyticDestructionRandomParamWithModel) =
        inherit RateModel<CatalyticDestructionRandomParamWithModel, CatalyticDestructionReaction>(p)

        let calculateCatSynthRates rnd t (CatalyticDestructionReaction (s, c)) =
            {
                reaction = s
                catalyst = c
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticDestructionReaction
                getBaseRates = p.destructionModel.getRates rnd
                eeParams = p.catDestrRndParam.catDestrRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateCatSynthRates rnd t) r


    type CatalyticDestructionSimilarParamWithModel =
        {
            catDestrSimParam : CatRatesSimilarityParam
            aminoAcids : list<AminoAcid>
            catDestrModel : CatalyticDestructionRandomModel
        }


    type CatalyticDestructionParamWithModel =
        | CatDestrRndParamWithModel of CatalyticDestructionRandomParamWithModel
        | CatDestrSimParamWithModel of CatalyticDestructionSimilarParamWithModel


    type CatalyticDestructionSimilarModel (p : CatalyticDestructionSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (CatalyticDestructionReaction (s, c)) = 
            let (DestructionReaction a) = s
            {
                reaction = s
                catalyst = c
                aminoAcids = p.aminoAcids
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticDestructionReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> a.createSameChirality e |> DestructionReaction)
                getBaseRates = p.catDestrModel.inputParams.destructionModel.getRates rnd
                getBaseCatRates = p.catDestrModel.getRates rnd t
                simParams = p.catDestrSimParam
                eeParams = p.catDestrModel.inputParams.catDestrRndParam.catDestrRndEeParams
                rateDictionary = p.catDestrModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateSimRates

        member __.getRates rnd t r = calculateSimRatesImpl rnd t r
        member __.inputParams = p
        member __.getAllRates() = getAllRatesImpl p.catDestrModel.rateDictionary


    type CatalyticDestructionModel =
        | CatDestrRndModel of CatalyticDestructionRandomModel
        | CatDestrSimModel of CatalyticDestructionSimilarModel

        member model.getRates rnd t r =
            match model with
            | CatDestrRndModel m -> m.getRates rnd t r
            | CatDestrSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | CatDestrRndModel m -> m.inputParams |> CatDestrRndParamWithModel
            | CatDestrSimModel m -> m.inputParams |> CatDestrSimParamWithModel

        member model.getAllRates() =
            match model with
            | CatDestrRndModel m -> m.getAllRates()
            | CatDestrSimModel m -> m.getAllRates()

        static member create p =
            match p with 
            | CatDestrRndParamWithModel q -> CatalyticDestructionRandomModel q |> CatDestrRndModel
            | CatDestrSimParamWithModel q -> CatalyticDestructionSimilarModel q |> CatDestrSimModel


    type SedimentationDirectRandomModel (p : SedimentationDirectRandomParam) =
        inherit RateModel<SedimentationDirectRandomParam, SedimentationDirectReaction>(p)

        let calculateRates rnd t _ =
            let k =
                match t with
                | BruteForce -> p.sedimentationDirectDistribution.nextDoubleOpt rnd
                | RandomChoice -> p.sedimentationDirectDistribution.nextDouble rnd |> Some
            getForwardRates (p.forwardScale, k)

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd t) r


    type SedimentationDirectModel =
        | SedDirRndModel of SedimentationDirectRandomModel

        member model.getRates rnd t r =
            match model with
            | SedDirRndModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | SedDirRndModel m -> m.inputParams |> SedDirRndParam

        member model.getAllRates() =
            match model with
            | SedDirRndModel m -> m.getAllRates()

        static member create p =
            match p with 
            | SedDirRndParam q -> SedimentationDirectRandomModel q |> SedDirRndModel


    type SedimentationAllRandomModel (p : SedimentationAllRandomParam) =
        inherit RateModel<SedimentationAllRandomParam, SedimentationAllReaction>(p)
        let calculateRates rnd _ = getForwardRates (p.forwardScale, p.sedimentationAllDistribution.nextDouble rnd |> Some)
        member model.getRates rnd r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd) r


    type SedimentationAllModel =
        | SedAllRndModel of SedimentationAllRandomModel

        member model.getRates rnd r =
            match model with
            | SedAllRndModel m -> m.getRates rnd r

        member model.inputParams =
            match model with
            | SedAllRndModel m -> m.inputParams |> SedAllRndParam

        member model.getAllRates() =
            match model with
            | SedAllRndModel m -> m.getAllRates()

        static member create p =
            match p with 
            | SedAllRndParam q -> SedimentationAllRandomModel q |> SedAllRndModel


    type LigationRandomModel (p : LigationRandomParam) =
        inherit RateModel<LigationRandomParam, LigationReaction>(p)

        let calculateRates rnd _ =
            let d = p.ligationDistribution
            getRates (p.forwardScale, d.nextDouble rnd |> Some) (p.backwardScale, d.nextDouble rnd |> Some)

        member model.getRates rnd r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd) r


    type LigationModel =
        | LigRndModel of LigationRandomModel

        member model.getRates rnd r =
            match model with
            | LigRndModel m -> m.getRates rnd r

        member model.inputParams =
            match model with
            | LigRndModel m -> m.inputParams |> LigRndParam

        member model.getAllRates() =
            match model with
            | LigRndModel m -> m.getAllRates()

        static member create p =
            match p with 
            | LigRndParam q -> LigationRandomModel q |> LigRndModel


    type CatalyticLigationRandomParamWithModel =
        {
            catLigationParam : CatalyticLigationRandomParam
            ligationModel : LigationModel
        }


    type CatalyticLigationParamWithModel =
        | CatLigRndParamWithModel of CatalyticLigationRandomParamWithModel

        member p.catLigationParam =
            match p with 
            | CatLigRndParamWithModel q -> q.catLigationParam


    type CatalyticLigationRandomModel (p : CatalyticLigationRandomParamWithModel) =
        inherit RateModel<CatalyticLigationRandomParamWithModel, CatalyticLigationReaction>(p)

        let calculateCatSynthRates rnd t (CatalyticLigationReaction (s, c)) =
            {
                reaction = s
                catalyst = c
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticLigationReaction
                getBaseRates = p.ligationModel.getRates rnd
                eeParams = p.catLigationParam.catLigRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateCatSynthRates rnd t) r


    type CatalyticLigationModel =
        | CatLigRndModel of CatalyticLigationRandomModel

        member model.getRates rnd t r =
            match model with
            | CatLigRndModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | CatLigRndModel m -> m.inputParams |> CatLigRndParamWithModel

        member model.getAllRates() =
            match model with
            | CatLigRndModel m -> m.getAllRates()

        static member create p =
            match p with 
            | CatLigRndParamWithModel q -> CatalyticLigationRandomModel q |> CatLigRndModel


    type RacemizationRandomModel (p : RacemizationRandomParam) =
        inherit RateModel<RacemizationRandomParam, RacemizationReaction>(p)

        let calculateRates rnd _ =
            let d = p.racemizationDistribution
            getRates (p.forwardScale, d.nextDouble rnd |> Some) (None, None)

        member model.getRates rnd r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd) r


    type RacemizationModel =
        | RacemRndModel of RacemizationRandomModel

        member model.getRates rnd r =
            match model with
            | RacemRndModel m -> m.getRates rnd r

        member model.inputParams =
            match model with
            | RacemRndModel m -> m.inputParams |> RacemRndParam

        member model.getAllRates() =
            match model with
            | RacemRndModel m -> m.getAllRates()

        static member create p =
            match p with 
            | RacemRndParam q -> RacemizationRandomModel q |> RacemRndModel


    type CatalyticRacemizationRandomParamWithModel =
        {
            catRacemRndParam : CatalyticRacemizationRandomParam
            racemizationModel : RacemizationModel
            aminoAcids : list<AminoAcid>
        }


    type CatalyticRacemizationRandomModel (p : CatalyticRacemizationRandomParamWithModel) =
        inherit RateModel<CatalyticRacemizationRandomParamWithModel, CatalyticRacemizationReaction>(p)

        let calculateCatSynthRates rnd t (CatalyticRacemizationReaction (s, c)) =
            {
                reaction = s
                catalyst = c
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticRacemizationReaction
                getBaseRates = p.racemizationModel.getRates rnd
                eeParams = p.catRacemRndParam.catRacemRndEeParams
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateCatRates

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateCatSynthRates rnd t) r


    type CatalyticRacemizationSimilarParamWithModel =
        {
            catRacemSimParam : CatRatesSimilarityParam
            aminoAcids : list<AminoAcid>
            catRacemModel : CatalyticRacemizationRandomModel
        }


    type CatalyticRacemizationParamWithModel =
        | CatRacemRndParamWithModel of CatalyticRacemizationRandomParamWithModel
        | CatRacemSimParamWithModel of CatalyticRacemizationSimilarParamWithModel


    type CatalyticRacemizationSimilarModel (p : CatalyticRacemizationSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (CatalyticRacemizationReaction (s, c)) =
            let (RacemizationReaction a) = s
            {
                reaction = s
                catalyst = c
                aminoAcids = p.aminoAcids
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticRacemizationReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> a.createSameChirality e |> RacemizationReaction)
                getBaseRates = p.catRacemModel.inputParams.racemizationModel.getRates rnd
                getBaseCatRates = p.catRacemModel.getRates rnd t
                simParams = p.catRacemSimParam
                eeParams = p.catRacemModel.inputParams.catRacemRndParam.catRacemRndEeParams
                rateDictionary = p.catRacemModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateSimRates

        member __.getRates rnd t r = calculateSimRatesImpl rnd t r
        member __.inputParams = p
        member __.getAllRates() = getAllRatesImpl p.catRacemModel.rateDictionary


    type CatalyticRacemizationModel =
        | CatRacemRndModel of CatalyticRacemizationRandomModel
        | CatRacemSimModel of CatalyticRacemizationSimilarModel

        member model.getRates rnd t r =
            match model with
            | CatRacemRndModel m -> m.getRates rnd t r
            | CatRacemSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | CatRacemRndModel m -> m.inputParams |> CatRacemRndParamWithModel
            | CatRacemSimModel m -> m.inputParams |> CatRacemSimParamWithModel

        member model.getAllRates() =
            match model with
            | CatRacemRndModel m -> m.getAllRates()
            | CatRacemSimModel m -> m.getAllRates()

        static member create p =
            match p with 
            | CatRacemRndParamWithModel q -> CatalyticRacemizationRandomModel q |> CatRacemRndModel
            | CatRacemSimParamWithModel q -> CatalyticRacemizationSimilarModel q |> CatRacemSimModel


    //[<CustomEquality>]
    //[<CustomComparison>]
    type ReactionRateModel =
        | FoodCreationRateModel of FoodCreationModel
        | WasteRemovalRateModel of WasteRemovalModel
        | WasteRecyclingRateModel of WasteRecyclingModel
        | SynthesisRateModel of SynthesisModel
        | DestructionRateModel of DestructionModel
        | CatalyticSynthesisRateModel of CatalyticSynthesisModel
        | CatalyticDestructionRateModel of CatalyticDestructionModel
        | LigationRateModel of LigationModel
        | CatalyticLigationRateModel of CatalyticLigationModel
        | SedimentationDirectRateModel of SedimentationDirectModel
        | SedimentationAllRateModel of SedimentationAllModel
        | RacemizationRateModel of RacemizationModel
        | CatalyticRacemizationRateModel of CatalyticRacemizationModel

        //member rm.name =
        //    match rm with
        //    | FoodCreationRateModel _ -> "FoodCreationRateModel"
        //    | WasteRemovalRateModel _ -> "WasteRemovalRateModel"
        //    | WasteRecyclingRateModel _ -> "WasteRecyclingRateModel"
        //    | SynthesisRateModel _ -> "SynthesisRateModel"
        //    | DestructionRateModel _ -> "DestructionRateModel"
        //    | CatalyticSynthesisRateModel v ->
        //        match v with
        //        | CatSynthRndModel _ -> "CatSynthRndModel"
        //        | CatSynthSimModel _ -> "CatSynthSimModel"
        //    | CatalyticDestructionRateModel v ->
        //        match v with
        //        | CatDestrRndModel _ -> "CatDestrRndModel"
        //        | CatDestrSimModel _ -> "CatDestrSimModel"
        //    | LigationRateModel _ -> "LigationRateModel"
        //    | CatalyticLigationRateModel v ->
        //        match v with
        //        | CatLigRndModel _ -> "CatLigRndModel"
        //    | SedimentationDirectRateModel _ -> "SedimentationDirectRateModel"
        //    | SedimentationAllRateModel _ -> "SedimentationAllRateModel"
        //    | RacemizationRateModel _ -> "RacemizationRateModel"
        //    | CatalyticRacemizationRateModel v ->
        //        match v with
        //        | CatRacemRndModel _ -> "CatRacemRndModel"
        //        | CatRacemSimModel _ -> "CatRacemSimModel"

        //member rm.inputParams =
        //    match rm with
        //    | FoodCreationRateModel m -> m.inputParams |> FoodCreationRateParam
        //    | WasteRemovalRateModel m -> m.inputParams |> WasteRemovalRateParam
        //    | WasteRecyclingRateModel m -> m.inputParams |> WasteRecyclingRateParam
        //    | SynthesisRateModel m -> m.inputParams |> SynthesisRateParam
        //    | DestructionRateModel m -> m.inputParams |> DestructionRateParam
        //    | CatalyticSynthesisRateModel v ->
        //        match v with 
        //        | CatSynthRndModel m -> m.inputParams.catSynthRndParam |> CatSynthRndParam |> CatalyticSynthesisRateParam
        //        | CatSynthSimModel m -> m.inputParams.catSynthSimParam |> CatSynthSimParam |> CatalyticSynthesisRateParam
        //    | CatalyticDestructionRateModel v ->
        //        match v with 
        //        | CatDestrRndModel m -> m.inputParams.catDestrRndParam |> CatDestrRndParam |> CatalyticDestructionRateParam
        //        | CatDestrSimModel m -> m.inputParams.catDestrSimParam |> CatDestrSimParam |> CatalyticDestructionRateParam
        //    | LigationRateModel m -> m.inputParams |> LigationRateParam
        //    | CatalyticLigationRateModel v ->
        //        match v with 
        //        | CatLigRndModel m -> m.inputParams.catLigationParam |> CatLigRndParam |> CatalyticLigationRateParam
        //    | SedimentationDirectRateModel m -> m.inputParams |> SedimentationDirectRateParam
        //    | SedimentationAllRateModel m -> m.inputParams |> SedimentationAllRateParam
        //    | RacemizationRateModel m -> m.inputParams |> RacemizationRateParam
        //    | CatalyticRacemizationRateModel v ->
        //        match v with 
        //        | CatRacemRndModel m -> m.inputParams.catRacemRndParam |> CatRacemRndParam |> CatalyticRacemizationRateParam
        //        | CatRacemSimModel m -> m.inputParams.catRacemSimParam |> CatRacemSimParam |> CatalyticRacemizationRateParam

        //member rm.dependsOn =
        //    match rm with
        //    | FoodCreationRateModel _ -> []
        //    | WasteRemovalRateModel _ -> []
        //    | WasteRecyclingRateModel _ -> []
        //    | SynthesisRateModel _ -> []
        //    | DestructionRateModel _ -> []
        //    | CatalyticSynthesisRateModel v ->
        //        match v with
        //        | CatSynthRndModel m -> [ m.inputParams.synthesisModel |> SynthesisRateModel ]
        //        | CatSynthSimModel m -> [ m.inputParams.catSynthModel |> CatSynthRndModel |> CatalyticSynthesisRateModel ]
        //    | CatalyticDestructionRateModel v ->
        //        match v with
        //        | CatDestrRndModel m -> [ m.inputParams.destructionModel |> DestructionRateModel ]
        //        | CatDestrSimModel m -> [ m.inputParams.catDestrModel |> CatDestrRndModel |> CatalyticDestructionRateModel ]
        //    | LigationRateModel _ -> []
        //    | CatalyticLigationRateModel v -> 
        //        match v with
        //        | CatLigRndModel m -> [ m.inputParams.ligationModel |> LigationRateModel ]
        //    | SedimentationDirectRateModel _ -> []
        //    | SedimentationAllRateModel _ -> []
        //    | RacemizationRateModel _ -> []
        //    | CatalyticRacemizationRateModel v ->
        //        match v with
        //        | CatRacemRndModel m -> [ m.inputParams.racemizationModel |> RacemizationRateModel ]
        //        | CatRacemSimModel m -> [ m.inputParams.catRacemModel |> CatRacemRndModel |> CatalyticRacemizationRateModel ]

        member rm.getAllRates() =
            match rm with
            | FoodCreationRateModel m -> m.getAllRates() |> FoodCreationRates
            | WasteRemovalRateModel m -> m.getAllRates() |> WasteRemovalRates
            | WasteRecyclingRateModel m -> m.getAllRates() |> WasteRecyclingRates
            | SynthesisRateModel m -> m.getAllRates() |> SynthesisRates
            | DestructionRateModel m -> m.getAllRates() |> DestructionRates
            | CatalyticSynthesisRateModel m -> m.getAllRates() |> CatalyticSynthesisRates
            | CatalyticDestructionRateModel m -> m.getAllRates() |> CatalyticDestructionRates
            | LigationRateModel m -> m.getAllRates() |> LigationRates
            | CatalyticLigationRateModel m -> m.getAllRates() |> CatalyticLigationRates
            | SedimentationDirectRateModel m -> m.getAllRates() |> SedimentationDirectRates
            | SedimentationAllRateModel m -> m.getAllRates() |> SedimentationAllRates
            | RacemizationRateModel m -> m.getAllRates() |> RacemizationRates
            | CatalyticRacemizationRateModel m -> m.getAllRates() |> CatalyticRacemizationRates

        //override this.Equals (o: obj) =
        //    match o with
        //    | :? ReactionRateModel as rm -> this.inputParams = rm.inputParams
        //    | _ -> false

        //override this.GetHashCode() = hash (this.name, this.inputParams)

        //interface IEquatable<ReactionRateModel> with
        //    member this.Equals(that : ReactionRateModel) = this.Equals(that)

        //interface IComparable with
        //    member this.CompareTo(thatObj) =
        //        match thatObj with
        //        | :? ReactionRateModel as that ->
        //            compare (this.name, this.inputParams) (that.name, that.inputParams)
        //        | _ ->
        //            raise <| ArgumentException("Can't compare instances of different types.")


    //let rec allDep (rm : ReactionRateModel) (acc : list<ReactionRateModel>) =
    //    match rm.dependsOn with 
    //    | [] -> acc
    //    | l -> l |> List.fold (fun a r -> allDep r (r :: a)) acc


    type ReactionRateModelWithUsage =
        {
            model : ReactionRateModel
            usage : ReactionRateModelParamUsage
        }


    //type ReactionRateProviderParams =
    //    {
    //        rateModels: list<ReactionRateModel>
    //    }

    //    member p.tryFindFoodCreationModel() = p.rateModels |> List.tryPick (fun e -> match e with | FoodCreationRateModel m -> Some m | _ -> None)
    //    member p.tryFindWasteRemovalModel() = p.rateModels |> List.tryPick (fun e -> match e with | WasteRemovalRateModel m -> Some m | _ -> None)
    //    member p.tryFindWasteRecyclingModel() = p.rateModels |> List.tryPick (fun e -> match e with | WasteRecyclingRateModel m -> Some m | _ -> None)
    //    member p.tryFindSynthesisModel() = p.rateModels |> List.tryPick (fun e -> match e with | SynthesisRateModel m -> Some m | _ -> None)
    //    member p.tryFindDestructionModel() = p.rateModels |> List.tryPick (fun e -> match e with | DestructionRateModel m -> Some m | _ -> None)
    //    member p.tryFindCatalyticSynthesisModel() = p.rateModels |> List.tryPick (fun e -> match e with | CatalyticSynthesisRateModel m -> Some m | _ -> None)
    //    member p.tryFindCatalyticDestructionModel() = p.rateModels |> List.tryPick (fun e -> match e with | CatalyticDestructionRateModel m -> Some m | _ -> None)
    //    member p.tryFindLigationModel() = p.rateModels |> List.tryPick (fun e -> match e with | LigationRateModel m -> Some m | _ -> None)
    //    member p.tryFindCatalyticLigationModel() = p.rateModels |> List.tryPick (fun e -> match e with | CatalyticLigationRateModel m -> Some m | _ -> None)
    //    member p.tryFindSedimentationDirectModel() = p.rateModels |> List.tryPick (fun e -> match e with | SedimentationDirectRateModel m -> Some m | _ -> None)
    //    member p.tryFindSedimentationAllModel() = p.rateModels |> List.tryPick (fun e -> match e with | SedimentationAllRateModel m -> Some m | _ -> None)
    //    member p.tryFindRacemizationModel() = p.rateModels |> List.tryPick (fun e -> match e with | RacemizationRateModel m -> Some m | _ -> None)
    //    member p.tryFindCatalyticRacemizationModel() = p.rateModels |> List.tryPick (fun e -> match e with | CatalyticRacemizationRateModel m -> Some m | _ -> None)

    //    member p.allModels =
    //        let prim = p.rateModels |> Set.ofList
    //        let dep = Set.difference (p.rateModels |> List.map (fun e -> allDep e []) |> List.concat |> Set.ofList) prim

    //        (prim |> Set.map (fun e -> { model = e; usage = PrimaryParam }))
    //        |> Set.union (dep |> Set.map (fun e -> { model = e; usage = DependsOnParam }))
    //        |> Set.toList

    //    member p.allParams = p.allModels |> List.map (fun e -> { modelParam = e.model.inputParams; usage = e.usage }) |> List.sort


    type ReactionRateProvider (p: ReactionRateProviderParams) =
        let getRatesImpl rnd t a =
            match a with
            | FoodCreation r -> p.tryFindFoodCreationParam() |> bind (fun m -> m.getRates r)
            | WasteRemoval r -> p.tryFindWasteRemovalModel() |> bind (fun m -> m.getRates r)
            | WasteRecycling r -> p.tryFindWasteRecyclingModel() |> bind (fun m -> m.getRates r)
            | Synthesis r -> p.tryFindSynthesisModel() |> bind (fun m -> m.getRates rnd r)
            | Destruction r -> p.tryFindDestructionModel() |> bind (fun m -> m.getRates rnd r)
            | CatalyticSynthesis r -> p.tryFindCatalyticSynthesisModel() |> bind (fun m -> m.getRates rnd t r)
            | CatalyticDestruction r -> p.tryFindCatalyticDestructionModel() |> bind (fun m -> m.getRates rnd t r)
            | Ligation r -> p.tryFindLigationModel() |> bind (fun m -> m.getRates rnd r)
            | CatalyticLigation r -> p.tryFindCatalyticLigationModel() |> bind (fun m -> m.getRates rnd t r)
            | SedimentationDirect r -> p.tryFindSedimentationDirectModel() |> bind (fun m -> m.getRates rnd t r)
            | SedimentationAll r -> p.tryFindSedimentationAllModel() |> bind (fun m -> m.getRates rnd r)
            | Racemization r -> p.tryFindRacemizationModel() |> bind (fun m -> m.getRates rnd r)
            | CatalyticRacemization r -> p.tryFindCatalyticRacemizationModel() |> bind (fun m -> m.getRates rnd t r)

        let getModelImpl n =
            match n with
            | FoodCreationName -> p.tryFindFoodCreationModel() |> Option.bind(fun e -> FoodCreationRateModel e |> Some)
            | WasteRemovalName -> p.tryFindWasteRemovalModel() |> Option.bind(fun e -> WasteRemovalRateModel e |> Some)
            | WasteRecyclingName -> p.tryFindWasteRecyclingModel() |> Option.bind(fun e -> WasteRecyclingRateModel e |> Some)
            | SynthesisName -> p.tryFindSynthesisModel() |> Option.bind(fun e -> SynthesisRateModel e |> Some)
            | DestructionName -> p.tryFindDestructionModel() |> Option.bind(fun e -> DestructionRateModel e |> Some)
            | CatalyticSynthesisName -> p.tryFindCatalyticSynthesisModel() |> Option.bind(fun e -> CatalyticSynthesisRateModel e |> Some)
            | CatalyticDestructionName -> p.tryFindCatalyticDestructionModel() |> Option.bind(fun e -> CatalyticDestructionRateModel e |> Some)
            | LigationName -> p.tryFindLigationModel() |> Option.bind(fun e -> LigationRateModel e |> Some)
            | CatalyticLigationName -> p.tryFindCatalyticLigationModel() |> Option.bind(fun e -> CatalyticLigationRateModel e |> Some)
            | SedimentationDirectName -> p.tryFindSedimentationDirectModel() |> Option.bind(fun e -> SedimentationDirectRateModel e |> Some)
            | SedimentationAllName -> p.tryFindSedimentationAllModel() |> Option.bind(fun e -> SedimentationAllRateModel e |> Some)
            | RacemizationName -> p.tryFindRacemizationModel() |> Option.bind(fun e -> RacemizationRateModel e |> Some)
            | CatalyticRacemizationName -> p.tryFindCatalyticRacemizationModel() |> Option.bind(fun e -> CatalyticRacemizationRateModel e |> Some)

        member __.providerParams = p
        member __.getRates rnd a = getRatesImpl rnd a
        member __.getModel n = getModelImpl n
        member __.getAllRates() = p.rateModels |> List.map (fun m -> m.getAllRates())
