namespace ClmImpure

open System.Collections.Generic
open FSharp.Collections

open Clm.Substances
open Clm.Distributions
open Clm.ReactionRates
open Clm.ReactionTypes


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

        if d.Count > 0 && (d.Count % 1_000_000) = 0
        then printfn "updatePrimaryReactions::d.Count = %A for type: %A. Something is not right." d.Count (typedefof<'R>)

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


            // TODO kk:20191014 - Here we need to ensure that number of successes in the call to i.simParams.simBaseDistribution.isDefined is NOT random but fixed. 
            // This is not that straightforward. 
            // Breaking the compilation for the time being...
            return! 0
            // Remove that stuff above one the issue is fixed.

            i.aminoAcids
            |> List.map (fun a -> i.simReactionCreator a, i.simParams.simBaseDistribution.isDefined i.rnd)
            |> List.map (fun (e, b) -> calculateCatRates e i.catalyst (getEeParams b))
            |> ignore

        cr


    let calculateSedDirSimRates (i : SedDirRatesSimInfo) =
        let r = (i.sedDirRatesInfo.sedFormingSubst, i.sedDirRatesInfo.sedDirAgent) |> SedimentationDirectReaction
        let re = (i.sedDirRatesInfo.sedFormingSubst, i.sedDirRatesInfo.sedDirAgent.enantiomer) |> SedimentationDirectReaction

        let cr = i.sedDirRatesInfo.getBaseRates r

        let calculateSedDirRates s c ee =
            let reaction = (s, c) |> SedimentationDirectReaction
            let related = calculateSedDirRates { i.sedDirRatesInfo with sedFormingSubst = s; sedDirAgent = c; eeParams = ee }
            updateRelatedReactions i.rateDictionary (fun e -> e.enantiomer) reaction related

        match cr.forwardRate with
        | None ->
            i.aminoAcids
            |> List.map (fun a -> i.reagents.[a])
            //|> List.choose id
            |> List.concat
            |> List.map (fun e -> calculateSedDirRates e i.sedDirRatesInfo.sedDirAgent SedDirRatesEeParam.defaultValue)
            |> ignore
        | Some (ReactionRate a) ->
            let cre = re |> i.sedDirRatesInfo.getBaseRates

            let m =
                match i.sedDirRatesInfo.eeParams.sedDirRateMultiplierDistr.value with
                | Some v -> v.mean
                | None -> 1.0

            let rateMult =
                match cre.forwardRate with
                | Some (ReactionRate b) ->(a + b) / 2.0 / m
                | _ -> failwith "calculateSedDirSimRates::calculateCatRates::FUBAR #1..."

            let getEeParams d =
                match d with
                | true ->
                    {
                        sedDirRateMultiplierDistr = i.simParams.getRateMultiplierDistr.getDistr None rateMult
                        eeForwardDistribution = i.simParams.getForwardEeDistr.getDistr cr.forwardRate cre.forwardRate
                    }
                | false -> SedDirRatesEeParam.defaultValue

            i.aminoAcids
            |> List.map (fun a -> i.reagents.[a], i.simParams.sedDirSimBaseDistribution.isDefined i.sedDirRatesInfo.rnd)
            //|> List.map (fun (e, b) -> e |> Option.bind (fun x -> Some (x, b)))
            //|> List.choose id
            |> List.map (fun (e, b) -> e |> List.map (fun a -> (a, b)))
            |> List.concat
            |> List.map (fun (e, b) -> calculateSedDirRates e i.sedDirRatesInfo.sedDirAgent (getEeParams b))
            |> ignore

        cr


    let getAllRatesImpl (d : Dictionary<'R, RateData>) =
        d
        |> Seq.map (|KeyValue|)
        |> List.ofSeq
        |> List.map (fun (r, d) -> { reaction = r; rateData = d })
