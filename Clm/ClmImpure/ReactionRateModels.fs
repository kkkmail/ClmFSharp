namespace ClmImpure

open System.Collections.Generic
open FSharp.Collections
open Clm.Substances
open Clm.ReactionTypes
open Clm.ReactionRates

open ClmImpure.ReactionRateFunctions

module ReactionRateModels =

    [<AbstractClass>]
    type RateModel<'P, 'R when 'R : equality> (p : 'P) =
        let rateDictionaryImpl = new Dictionary<'R, RateData>()
        member _.rateDictionary = rateDictionaryImpl
        member _.inputParams = p

        member _.getAllRates() = getAllRatesImpl rateDictionaryImpl


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
                getReactionData = fun _ -> p.aminoAcids
                getMatchingReactionMult = fun x -> x
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticSynthesisReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> [ a.createSameChirality e |> SynthesisReaction ])
                getBaseRates = p.catSynthModel.inputParams.synthesisModel.getRates rnd
                getBaseCatRates = p.catSynthModel.getRates rnd t
                simParams = p.catSynthSimParam
                eeParams = p.catSynthModel.inputParams.catSynthRndParam.catSynthRndEeParams
                rateDictionary = p.catSynthModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.catSynthModel.rateDictionary


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
                getReactionData = fun _ -> p.aminoAcids
                getMatchingReactionMult = fun x -> x
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticDestructionReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> [ a.createSameChirality e |> DestructionReaction ])
                getBaseRates = p.catDestrModel.inputParams.destructionModel.getRates rnd
                getBaseCatRates = p.catDestrModel.getRates rnd t
                simParams = p.catDestrSimParam
                eeParams = p.catDestrModel.inputParams.catDestrRndParam.catDestrRndEeParams
                rateDictionary = p.catDestrModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.catDestrModel.rateDictionary


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
                | RandomChoice -> p.sedDirDistribution.nextDouble rnd |> Some
            getForwardRates (p.forwardScale, k)

        member model.getRates rnd t r = getRatesImpl model.rateDictionary getEnantiomer (calculateRates rnd t) r


    type SedimentationDirectSimilarParamWithModel =
        {
            sedDirSimParam : SedDirSimilarityParam
            aminoAcids : list<AminoAcid>
            reagents : Map<AminoAcid, list<SedDirReagent>>
            sedDirModel : SedimentationDirectRandomModel
        }


    type SedimentationDirectSimilarModel (p : SedimentationDirectSimilarParamWithModel) =
        inherit RateModel<SedimentationDirectSimilarParamWithModel, SedimentationDirectReaction>(p)

        let calculateSimRatesImpl rnd t (SedimentationDirectReaction (s, c)) =
            {
                sedDirRatesInfo =
                    {
                        sedFormingSubst = s
                        sedDirAgent = c
                        getBaseRates = p.sedDirModel.getRates rnd t
                        eeParams = p.sedDirModel.inputParams.sedDirRatesEeParam
                        rateGenerationType = t
                        rnd = rnd
                    }

                aminoAcids = p.aminoAcids
                reagents = p.reagents
                simParams = p.sedDirSimParam
                rateDictionary = p.sedDirModel.rateDictionary
            }
            |> calculateSedDirSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.sedDirModel.rateDictionary


    type SedimentationDirectParamWithModel =
        | SedDirRndParamWithModel of SedimentationDirectRandomParam
        | SedDiSimParamWithModel of SedimentationDirectSimilarParamWithModel


    type SedimentationDirectModel =
        | SedDirRndModel of SedimentationDirectRandomModel
        | SedDirSimModel of SedimentationDirectSimilarModel

        member model.getRates rnd t r =
            match model with
            | SedDirRndModel m -> m.getRates rnd t r
            | SedDirSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | SedDirRndModel m -> m.inputParams |> SedDirRndParamWithModel
            | SedDirSimModel m -> m.inputParams |> SedDiSimParamWithModel

        member model.getAllRates() =
            match model with
            | SedDirRndModel m -> m.getAllRates()
            | SedDirSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | SedDirRndParamWithModel q -> SedimentationDirectRandomModel q |> SedDirRndModel
            | SedDiSimParamWithModel q -> SedimentationDirectSimilarModel q |> SedDirSimModel


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


    type CatalyticLigationSimilarParamWithModel =
        {
            catLigModel : CatalyticLigationRandomModel
            peptideBondData : PeptideBondData
            catLigSimParam : CatRatesSimilarityParam
        }


    type CatalyticLigationParamWithModel =
        | CatLigRndParamWithModel of CatalyticLigationRandomParamWithModel
        | CatLigSimParamWithModel of CatalyticLigationSimilarParamWithModel

//        member p.catLigationParam =
//            match p with
//            | CatLigRndParamWithModel q -> q.catLigationParam
//            | CatLigSimParamWithModel q -> q.catLigSimParam


    type CatalyticLigationSimilarModel (p : CatalyticLigationSimilarParamWithModel) =
        let calculateSimRatesImpl rnd t (CatalyticLigationReaction (s, c)) =
            let (LigationReaction a) = s
            {
                reaction = s
                catalyst = c
                getReactionData = fun r -> p.peptideBondData.findSameBondSymmetry r.peptideBond
                getMatchingReactionMult = fun x -> x
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticLigationReaction
                getCatReactEnantiomer = getEnantiomer

                simReactionCreator =
                    fun e ->
                        ((p.peptideBondData.findSameBond e) @ (p.peptideBondData.findSameBond s.peptideBond))
                        |> List.distinct

                getBaseRates = p.catLigModel.inputParams.ligationModel.getRates rnd
                getBaseCatRates = p.catLigModel.getRates rnd t
                simParams = p.catLigSimParam
                eeParams = p.catLigModel.inputParams.catLigationParam.catLigRndEeParams
                rateDictionary = p.catLigModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.catLigModel.rateDictionary


    type CatalyticLigationModel =
        | CatLigRndModel of CatalyticLigationRandomModel
        | CatLigSimModel of CatalyticLigationSimilarModel

        member model.getRates rnd t r =
            match model with
            | CatLigRndModel m -> m.getRates rnd t r
            | CatLigSimModel m -> m.getRates rnd t r

        member model.inputParams =
            match model with
            | CatLigRndModel m -> m.inputParams |> CatLigRndParamWithModel
            | CatLigSimModel m -> m.inputParams |> CatLigSimParamWithModel

        member model.getAllRates() =
            match model with
            | CatLigRndModel m -> m.getAllRates()
            | CatLigSimModel m -> m.getAllRates()

        static member create p =
            match p with
            | CatLigRndParamWithModel q -> CatalyticLigationRandomModel q |> CatLigRndModel
            | CatLigSimParamWithModel q -> CatalyticLigationSimilarModel q |> CatLigSimModel


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
                getReactionData = fun _ -> p.aminoAcids
                getMatchingReactionMult = fun x -> x
                getCatEnantiomer = getEnantiomer
                catReactionCreator = CatalyticRacemizationReaction
                getCatReactEnantiomer = getEnantiomer
                simReactionCreator = (fun e -> [ a.createSameChirality e |> RacemizationReaction ])
                getBaseRates = p.catRacemModel.inputParams.racemizationModel.getRates rnd
                getBaseCatRates = p.catRacemModel.getRates rnd t
                simParams = p.catRacemSimParam
                eeParams = p.catRacemModel.inputParams.catRacemRndParam.catRacemRndEeParams
                rateDictionary = p.catRacemModel.rateDictionary
                rateGenerationType = t
                rnd = rnd
            }
            |> calculateSimRates

        member _.getRates rnd t r = calculateSimRatesImpl rnd t r
        member _.inputParams = p
        member _.getAllRates() = getAllRatesImpl p.catRacemModel.rateDictionary


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


    type ReactionRateModelWithUsage =
        {
            model : ReactionRateModel
            usage : ReactionRateModelParamUsage
        }
