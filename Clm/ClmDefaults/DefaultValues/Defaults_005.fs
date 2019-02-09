namespace ClmDefaults

open Clm.ReactionRates
open Clm.ModelParams
open ClmDefaults.DefaultValuesExt

module Defaults_005 =

    let description = "Increased defaultCatDestrSimModel -> simThreshold to 0.35 in comparision to Defaults_001."

    let getDefaultRateModels numberOfAminoAcids =
        //===========================================================
        let foodModel = ReactionRateProvider.defaultFoodCreationModel 0.01
        let wasteModel = ReactionRateProvider.defaultWasteRemovalModel 10.0
        let wasteRecyclingModel = ReactionRateProvider.defaultWasteRecyclingModel 0.1
        //===========================================================
        let synthModel = ReactionRateProvider.defaultSynthRndModel (0.001, 0.001)
        //let catSynthRndParams = (synthModel, (Some 0.02), 1000.0)
        let catSynthRndParams = (synthModel, (Some 0.002), 10000.0)
        //let catSynthRndParams = (synthModel, (Some 0.0005), 1000.0)
        //let catSynthModel = ReactionRateProvider.defaultCatSynthRndModel catSynthRndParams
        let catSynthModel = ReactionRateProvider.defaultCatSynthSimModel catSynthRndParams (Some 0.3, numberOfAminoAcids)
        //===========================================================
        let destrModel = ReactionRateProvider.defaultDestrRndModel (0.001, 0.001)
        //let catDestrRndParams = (destrModel, (Some 0.02), 1000.0)
        let catDestrRndParams = (destrModel, (Some 0.002), 10000.0)
        //let catDestrRndParams = (destrModel, (Some 0.0005), 1000.0)
        //let catDestrModel = ReactionRateProvider.defaultCatDestrRndModel catDestrRndParams
        let catDestrModel = ReactionRateProvider.defaultCatDestrSimModel catDestrRndParams (Some 0.35, numberOfAminoAcids)
        //===========================================================
        //let ligModel = ReactionRateProvider.defaultLigRndModel (0.001, 0.0001)
        //let ligModel = ReactionRateProvider.defaultLigRndModel (1.0, 0.1)
        let ligModel = ReactionRateProvider.defaultLigRndModel (1.0, 1.0)
        let catLigModel = ReactionRateProvider.defaultCatLigRndModel (ligModel, (Some 0.00005), 2000.0)
        //===========================================================
        let sedDirModel = ReactionRateProvider.defaultSedDirRndModel (0.00001, 10000.0)
        let sedAllModel = ReactionRateProvider.defaultSedAllRndModel 0.1
        //===========================================================
        let racemModel = ReactionRateProvider.defaultRacemRndModel 0.001
        //let catRacemRndParams = (racemModel, (Some 0.02), 1000.0)
        let catRacemRndParams = (racemModel, (Some 0.0005), 1000.0)
        //let catRacemModel = ReactionRateProvider.defaultCatRacemRndModel catRacemRndParams
        let catRacemModel = ReactionRateProvider.defaultCatRacemSimModel catRacemRndParams (Some 0.2, numberOfAminoAcids)
        //===========================================================
        let rates =
            [
                //foodModel |> FoodCreationRateModel
                //wasteModel |> WasteRemovalRateModel
                wasteRecyclingModel |> WasteRecyclingRateModel

                synthModel |> SynthesisRateModel
                //catSynthModel |> CatalyticSynthesisRateModel

                destrModel |> DestructionRateModel
                catDestrModel |> CatalyticDestructionRateModel

                ligModel |> LigationRateModel
                //catLigModel |> CatalyticLigationRateModel

                //sedDirModel |> SedimentationDirectRateModel
                //sedAllModel |> SedimentationAllRateModel

                //racemModel |> RacemizationRateModel
                //catRacemModel |> CatalyticRacemizationRateModel
            ]
        //===========================================================

        {
            rateModels = rates
        }


    let defaultValue =
        {
            modelCommandLineParams =
                [
                    {
                        tEnd = 250_000.0m
                        y0 = 10.0m
                        useAbundant = false
                    }

                    {
                        tEnd = 250_000.0m
                        y0 = 5.0m
                        useAbundant = false
                    }

                    {
                        tEnd = 250_000.0m
                        y0 = 20.0m
                        useAbundant = false
                    }
                ]

            getDefaultRateModels = getDefaultRateModels
            description = Some description
        }
