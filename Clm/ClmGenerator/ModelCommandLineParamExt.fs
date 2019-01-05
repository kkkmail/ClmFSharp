namespace Clm.Generator

open System
open Clm.Substances
open Clm.Distributions
open Clm.ReactionRates
open Clm.ModelParams

module ModelCommandLineParamExt =

    type ModelCommandLineParam
        with

        static member defaultValues =
            [
                {
                    tEnd = 10_000.0
                    y0 = 10.0
                    useAbundant = None
                }

                {
                    tEnd = 100_000.0
                    y0 = 10.0
                    useAbundant = None
                }

                {
                    tEnd = 100_000.0
                    y0 = 5.0
                    useAbundant = None
                }

                {
                    tEnd = 100_000.0
                    y0 = 20.0
                    useAbundant = None
                }
            ]
