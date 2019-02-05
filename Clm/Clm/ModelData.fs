namespace Clm

open Clm.Substances
open Clm.Distributions
open Clm.ReactionRates
open Clm.DataLocation
open Clm.ModelParams

module ModelData =

    type LevelZero = array<double>
    type LevelOne = array<double * int>
    type LevelTwo = array<double * int * int>
    type LevelThree = array<double * int * int * int>


    type ModelIndices =
        {
            level0 : LevelZero
            level1 : LevelOne
            level2 : LevelTwo
            level3 : LevelThree
        }

        static member defaultValue =
            {
                level0 = [||]
                level1 = [||]
                level2 = [||]
                level3 = [||]
            }


    let calculateValue (indicies : ModelIndices) (x: double[]) =
        let mutable sum = 0.0

        for coeff in indicies.level0 do
            sum <- sum + coeff

        for (coeff, j1) in indicies.level1 do
            sum <- sum + coeff * x.[j1]

        for (coeff, j1, j2) in indicies.level2 do
            sum <- sum + coeff * x.[j1] * x.[j2]

        for (coeff, j1, j2, j3) in indicies.level3 do
            sum <- sum + coeff * x.[j1] * x.[j2] * x.[j3]

        sum


    type ModelData =
        {
            totalSubst : LevelOne
            totals : array<LevelOne * LevelOne>
            derivative : array<ModelIndices>

            // Not supported yet
            xSumN : LevelOne option
            xSumSquaredN : LevelTwo option
        }

        static member defaultValue =
            {
                totalSubst = [||]
                totals = [||]
                derivative = [||]
                xSumN = None
                xSumSquaredN = None
            }

        member md.getDerivative x = md.derivative |> Array.map (fun i -> calculateValue i x)
        member md.getTotalSubst x = calculateValue { ModelIndices.defaultValue with level1 = md.totalSubst } x

        member md.getTotals x =
            md.totals
            |> Array.map (fun (l, r) ->
                (calculateValue { ModelIndices.defaultValue with level1 = l } x),
                (calculateValue { ModelIndices.defaultValue with level1 = r } x)
                )

