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

            // TODO kk:20200531 - Rename into ??? in a separate PR.
            // Gets the list of "objects" to choose from.
            // For e.g. catalytic synthesis this is a list of amino acids.
            // However, for catalytic ligation this is a substantially more difficult question to answer and it my depend on the model.
            // That's the reason for making this label a function.
            aminoAcids : CatRatesSimilarityParam -> 'R -> list<'A>

            // Reactions that match given reaction by some parameters controlled by the model.
            // This is mostly for catalytic ligation where a model may require that catalyst is applied to the
            // same peptide bond.
            // All other reactions (e.g. catalytic synthesis) should return empty list.
            getMatchingReactions : CatRatesSimilarityParam -> 'R -> list<'R>

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


    let calculateCatRates i s c e =
        let reaction = (s, c) |> i.catReactionCreator
        let related = i.toCatRatesInfo s c e |> calculateCatRatesImpl
        updateRelatedReactions i.rateDictionary i.getCatReactEnantiomer reaction related


    let calculateMatchingRates<'A, 'R, 'C, 'RC> (i : CatRatesSimInfo<'A, 'R, 'C, 'RC>) (r : 'R) =
        let m =
            i.getMatchingReactions i.simParams r
            |> List.map (fun x -> calculateCatRates i s c e)

        r


    /// The logic here is as follows:
    ///     1. Given incoming base reaction (i.reaction), which was chosen by a random generator, we first get a list
    ///        of "matching" reactions except incoming reaction. For simple base reactions (e.g. synthesis) the list of
    ///        matching reactions is empty by definition. The order of such reactions is 1 (e.g. F -> A) and they simply
    ///        don't have extra "information" to go beyond a chosen base reaction. The distinction among incoming base
    ///        reaction and "matching" reactions currently seems important. But that may change.
    ///
    ///        The situation changes drastically when the base reaction is of order 2 and especially when considering
    ///        reactions like ligation, where the reaction binds two specific amino acids, while both may and usually
    ///        do have some chains of other amino acids on the left / right of bound amino acids.
    ///
    ///        The term "matching" is probably confusing. Please, refer to  PeptideBondMap for further information.
    ///        This is the value aa below.
    ///
    ///     2. We then call the relevant distribution function (i.getBaseCatRates). In the majority of cases it should
    ///        return some values. And if it returns none, then we should mark incoming catalytic reaction and all
    ///        "matching" catalytic reactions as having exactly zero (None) catalytic rate.
    ///
    ///     3. However if (i.getBaseCatRates) returns some values (and this is the standard scenario) then the following
    ///        should happen.
    ///
    ///     4. For each "matching" reaction apply ???TBD??? distribution
    ///
    ///     5. For each base + "matching" reaction apply similarity ...
    let calculateSimRates<'A, 'R, 'C, 'RC> (i : CatRatesSimInfo<'A, 'R, 'C, 'RC>) =
        let r = (i.reaction, i.catalyst) |> i.catReactionCreator
        let re = (i.reaction, i.getCatEnantiomer i.catalyst) |> i.catReactionCreator
        let br = i.getBaseRates i.reaction // (bf, bb)
        let cr = r |> i.getBaseCatRates // (f, b)

        // List of "objects" to choose from.
        // For e.g. catalytic synthesis this is a list of amino acids.
        // However, for catalytic ligation this is a substantially more difficult question to answer and it may depend on the model.
        // That's the reason for making this record label a function.
        let aa = i.aminoAcids i.simParams i.reaction

        let calculateCatRates = calculateCatRates i

        match (cr.forwardRate, cr.backwardRate) with
        | None, None ->
            aa
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

            let abcd =
                match i.simParams.catRatesSimGeneration with
                | DistributionBased simBaseDistribution ->
                    aa |> List.map (fun a -> i.simReactionCreator a, simBaseDistribution.isDefined i.rnd)
                | FixedValue d ->
                    // Here we need to ensure that number of successes is NOT random but fixed.
                    let isDefined j =
                        match d.value.distributionParams.threshold with
                        | Some t -> (double j) < t * (double aa.Length)
                        | None -> true

                    aa
                    |> List.map(fun a -> i.rnd.nextDouble(), a)
                    |> List.sortBy (fun (r, _) -> r)
                    |> List.mapi (fun j (_, a) -> i.simReactionCreator a, isDefined j)
                |> List.map (fun (e, b) -> calculateCatRates e i.catalyst (getEeParams b))
//                |> ignore

            abcd |> ignore
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
