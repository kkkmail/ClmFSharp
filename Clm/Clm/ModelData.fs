namespace Clm

open Clm.Substances
open Clm.Distributions
open Clm.ReactionRates
open Clm.Reactions
open Clm.DataLocation
open Clm.ModelParams
open Clm.ReactionTypes

module ModelData =

    type LevelZero = array<double>
    type LevelOne = array<double * int>
    type LevelTwo = array<double * int * int>
    type LevelThree = array<double * int * int * int>


    type ModelIndexInfo =
        | LevelZeroIndex of LevelZero
        | LevelOneIndex of LevelOne
        | LevelTwoIndex of LevelTwo
        | LevelThreeIndex of LevelThree

        //static member create (i : list<int>) v =
        //    match i with
        //    | [] -> [| v |] |> LevelZeroIndex
        //    | h :: t ->
        //        match t with
        //        | [] -> 


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


    type ModelCalculationData =
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

        static member create (m : Map<Substance, int>) (allReac : list<AnyReaction>) =
            let normalized = allReac |> List.map (fun e -> e.reaction.info.normalized(), e.forwardRate, e.backwardRate)

            let getRates chooser =
                normalized
                |> List.map (fun (r, f, b) -> r, chooser (f, b))
                |> List.choose (fun (e, r) -> r |> Option.bind (fun v -> Some (e, v)))

            let fwd =
                getRates fst
                |> List.map (fun (r, v) -> r.inputNormalized, r.outputNormalized, v)

            let bkw = getRates snd
            0


    type ModelData =
        {
            modelDataParams : ModelDataParams
            calculationData : ModelCalculationData
            allRawReactions : array<ReactionName * int>
            allReactions : array<ReactionName * int>
        }

