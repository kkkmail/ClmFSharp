namespace ClmImpure

open System.Collections.Generic
open FSharp.Collections

open Clm.Substances
open Clm.Distributions
open Clm.ReactionRates

module ReactionRateFunctions =

    let dictionaryToList (d : Dictionary<'R, (ReactionRate option * ReactionRate option)>) =
        d
        |> List.ofSeq
        |> List.map (fun e -> e.Key, e.Value)
        |> List.sortBy (fun (k, _) -> k)


    let updatePrimaryReactions<'R>
        (d : Dictionary<'R, RateData>)
        (getEnantiomer : 'R -> 'R)
        (primary : RateData)
        (r : 'R) =

        let enantiomer = getEnantiomer r
        if d.ContainsKey r |> not then d.Add(r, primary)
        if d.ContainsKey enantiomer |> not then d.Add(enantiomer, primary)


    let updateSimilarReactions<'R>
        (d : Dictionary<'R, RateData>)
        (getEnantiomer : 'R -> 'R)
        (similar : list<ReactionRateData<'R>>) =

        similar |> List.map (fun e -> if d.ContainsKey e.reaction |> not then d.Add(e.reaction, e.rateData)) |> ignore
        similar |> List.map (fun e -> if d.ContainsKey (getEnantiomer e.reaction) |> not then d.Add(getEnantiomer e.reaction, e.rateData)) |> ignore


    let updateRelatedReactions<'R>
        (d : Dictionary<'R, RateData>)
        (getEnantiomer : 'R -> 'R)
        (r : 'R)
        (x : RelatedReactions<'R>) =

        updatePrimaryReactions d getEnantiomer x.primary r
        updateSimilarReactions d getEnantiomer x.similar
        x.primary


    let getRatesImpl<'R> 
        (rateDictionary : Dictionary<'R, RateData>)
        (getEnantiomer : 'R -> 'R)
        (calculateRates : 'R -> RelatedReactions<'R>)
        (reaction : 'R)  =
        match rateDictionary.TryGetValue reaction with
        | true, rates -> rates
        | false, _ ->
            calculateRates reaction
            |> updateRelatedReactions rateDictionary getEnantiomer reaction


    //let inline getModelRates<'M, 'R when 'M : (member getRates : 'R -> (ReactionRate option * ReactionRate option))>
    //    (mo : 'M option) (r : 'R) : (ReactionRate option * ReactionRate option) =
    //    match mo with
    //    | Some m -> ((^M) : (member getRates : 'R -> (ReactionRate option * ReactionRate option)) (m, r))
    //    | None -> (None, None)
    //
    //
    //let inline getModelRates2<'M, 'R when 'M : (member getRates : RateGenerationType -> 'R -> (ReactionRate option * ReactionRate option))>
    //    (mo : 'M option) (t : RateGenerationType) (r : 'R) : (ReactionRate option * ReactionRate option) =
    //    match mo with
    //    | Some m -> ((^M) : (member getRates : RateGenerationType -> 'R -> (ReactionRate option * ReactionRate option)) (m, t, r))
    //    | None -> (None, None)


    type CatRatesSimInfo<'A, 'R, 'C, 'RC> =
        {
            reaction : 'R
            catalyst : 'C
            aminoAcids : list<'A>
            getCatEnantiomer : 'C -> 'C
            catReactionCreator : ('R * 'C) -> 'RC
            simReactionCreator : 'A -> 'R
            getCatReactEnantiomer : 'RC -> 'RC
            getBaseRates : 'R -> RateData // Get rates of base (not catalyzed) reaction.
            getBaseCatRates : 'RC -> RateData // Get rates of underlying catalyzed reaction.
            simParams : CatRatesSimilarityParam
            eeParams : CatRatesEeParam
            rateDictionary : Dictionary<'RC, RateData>
            rateGenerationType : RateGenerationType
            rnd : RandomValueGetter
        }

        member i.toCatRatesInfo r c e =
            {
                reaction = r
                catalyst = c
                getCatEnantiomer = i.getCatEnantiomer
                catReactionCreator = i.catReactionCreator
                getBaseRates = i.getBaseRates
                eeParams = e
                rateGenerationType = i.rateGenerationType
                rnd = i.rnd
            }


    let calculateSimRates<'R, 'C, 'RC> (i : CatRatesSimInfo<AminoAcid, 'R, 'C, 'RC>) =
        let r = (i.reaction, i.catalyst) |> i.catReactionCreator
        let re = (i.reaction, i.getCatEnantiomer i.catalyst) |> i.catReactionCreator
        let br = i.getBaseRates i.reaction // (bf, bb)
        let cr = r |> i.getBaseCatRates // (f, b)

        let calculateCatRates s c e =
            let reaction = (s, c) |> i.catReactionCreator
            let related = i.toCatRatesInfo s c e |> calculateCatRates
            updateRelatedReactions i.rateDictionary i.getCatReactEnantiomer reaction related

        match (cr.forwardRate, cr.backwardRate) with
        | None, None ->
            i.aminoAcids
            |> List.map (fun a -> i.simReactionCreator a)
            |> List.map (fun e -> calculateCatRates e i.catalyst CatRatesEeParam.defaultValue)
            |> ignore
        | _ ->
            let cre = re |> i.getBaseCatRates

            let rateMult =
                match cr.forwardRate, cre.forwardRate, cr.backwardRate, cre.backwardRate with
                | Some (ReactionRate a), Some (ReactionRate b), _, _ ->
                    match br.forwardRate with
                    | Some (ReactionRate c) -> (a + b) / 2.0 / c
                    | None -> failwith "calculateSimRates::calculateCatRates::FUBAR #1..."
                | _, _, Some (ReactionRate a), Some (ReactionRate b) ->
                    match br.backwardRate with
                    | Some (ReactionRate c) -> (a + b) / 2.0 / c
                    | None -> failwith "calculateSimRates::calculateCatRates::FUBAR #2..."
                | _ -> failwith "calculateSimRates::calculateCatRates::FUBAR #3..."

            let getEeParams d =
                match d with
                | true ->
                    {
                        rateMultiplierDistr = i.simParams.getRateMultiplierDistr.getDistr None rateMult
                        eeForwardDistribution = i.simParams.getForwardEeDistr.getDistr cr.forwardRate cre.forwardRate
                        eeBackwardDistribution = i.simParams.getBackwardEeDistr.getDistr cr.backwardRate cre.backwardRate
                    }
                | false -> CatRatesEeParam.defaultValue

            i.aminoAcids
            |> List.map (fun a -> i.simReactionCreator a, i.simParams.simBaseDistribution.isDefined i.rnd)
            |> List.map (fun (e, b) -> calculateCatRates e i.catalyst (getEeParams b))
            |> ignore

        cr


    let getAllRatesImpl (d : Dictionary<'R, RateData>) =
        d
        |> Seq.map (|KeyValue|)
        |> List.ofSeq
        |> List.map (fun (r, d) -> { reaction = r; rateData = d })
