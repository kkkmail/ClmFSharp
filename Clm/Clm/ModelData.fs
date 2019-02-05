namespace Clm

open Clm.Substances
open Clm.Distributions
open Clm.ReactionRates
open Clm.Reactions
open Clm.DataLocation
open Clm.ModelParams
open Clm.ReactionTypes

module ModelData =

    type LevelZero = double
    type LevelOne = double * int
    type LevelTwo = double * int * int
    type LevelThree = double * int * int * int


    type SubstUpdateInfo =
        | NoSubst
        | OneSubst of Substance
        | TwoSubst of Substance * Substance
        | ThreeSubst of Substance * Substance * Substance

        static member create i =
            match i with
            | [] -> NoSubst
            | h1 :: t1 ->
                match t1 with
                | [] -> h1 |> OneSubst
                | h2 :: t2 ->
                    match t2 with
                    | [] -> (h1, h2) |> TwoSubst
                    | h3 :: t3 ->
                        match t3 with
                        | [] -> (h1, h2, h3) |> ThreeSubst
                        | _ -> failwith (sprintf "SubstUpdateInfo: invalid input: %A" i)


    type ModelIndices =
        {
            level0 : array<LevelZero>
            level1 : array<LevelOne>
            level2 : array<LevelTwo>
            level3 : array<LevelThree>
        }

        static member defaultValue =
            {
                level0 = [||]
                level1 = [||]
                level2 = [||]
                level3 = [||]
            }

        static member create (m : Map<Substance, int>) (i : list<double * SubstUpdateInfo>) =
            let l0 =
                i
                |> List.map (fun (v, e) -> match e with | NoSubst -> Some v | _ -> None)
                |> List.choose id

            let l1 =
                i
                |> List.map (fun (v, e) -> match e with | OneSubst s1 -> Some (v, m.[s1]) | _ -> None)
                |> List.choose id

            let l2 =
                i
                |> List.map (fun (v, e) -> match e with | TwoSubst (s1, s2) -> Some (v, m.[s1], m.[s2]) | _ -> None)
                |> List.choose id

            let l3 =
                i
                |> List.map (fun (v, e) -> match e with | ThreeSubst (s1, s2, s3) -> Some (v, m.[s1], m.[s2], m.[s3]) | _ -> None)
                |> List.choose id

            {
                level0 = l0 |> Array.ofList
                level1 = l1 |> Array.ofList
                level2 = l2 |> Array.ofList
                level3 = l3 |> Array.ofList
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
            totalSubst : array<LevelOne>
            totals : array<array<LevelOne> * array<LevelOne>>
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

        static member createTotalSubst =
            failwith ""

        static member createTotals =
            failwith ""

        static member createDerivative noOfSubst (m : Map<Substance, int>) (allReac : list<AnyReaction>) =
            let normalized = allReac |> List.map (fun e -> e.reaction.info.normalized(), e.forwardRate, e.backwardRate)

            let processReaction i o (ReactionRate v) =
                let r = i |> SubstUpdateInfo.create

                (i |> List.map(fun e -> e, -1))
                @
                (o |> List.map(fun e -> e, 1))
                |> List.groupBy (fun (s, _) -> s)
                |> List.map (fun (s, e) -> s, e |> List.map (fun (_, i) -> i) |> List.sum)
                |> List.filter (fun (_, e) -> e <> 0)
                |> List.map (fun (s, m) -> s, ((double m) * v, r))

            let getRates chooser =
                normalized
                |> List.map (fun (r, f, b) -> r, chooser (f, b))
                |> List.choose (fun (e, r) -> r |> Option.bind (fun v -> Some (e, v)))

            let allMap =
                (getRates fst |> List.map (fun (r, v) -> processReaction r.inputNormalized r.outputNormalized v))
                @
                (getRates snd |> List.map (fun (r, v) -> processReaction r.outputNormalized r.inputNormalized v))
                |> List.concat
                |> List.groupBy (fun (s, _) -> s)
                |> List.map (fun (s, l) -> m.[s], l |> List.map (fun (_, e) -> e) |> ModelIndices.create m)
                |> Map.ofList

            [| for i in 0..(noOfSubst - 1) -> allMap.TryFind i |]
            |> Array.map (fun e -> match e with | Some v -> v | None -> ModelIndices.defaultValue)

        static member create noOfSubst (m : Map<Substance, int>) (allReac : list<AnyReaction>) =
            {
                totalSubst = failwith ""
                totals = failwith ""
                derivative = ModelCalculationData.createDerivative noOfSubst m allReac

                xSumN = None
                xSumSquaredN = None
            }


    type ModelData =
        {
            modelDataParams : ModelDataParams
            calculationData : ModelCalculationData
            allRawReactions : array<ReactionName * int>
            allReactions : array<ReactionName * int>
        }
