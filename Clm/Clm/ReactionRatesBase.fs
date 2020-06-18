namespace Clm

open FSharp.Collections
open Clm.Distributions
open Clm.ReactionTypes

module ReactionRatesBase =

    /// Specifies how to generate rates.
    /// RandomChoice first randomly determine the reactions with non-zero rates and then gets these rates (without using threshold).
    type RateGenerationType =
        | RandomChoice


    /// Specifies how to apply similarity.
    /// DistrBased uses distribution threshold to determine if an amino acid should / should not be included.
    /// This results in some spread in the number of amino acids.
    /// FixedVal - fixes the number of amino acids, but the choice of them is still random.
    type CatRatesSimGenType =
        | DistrBased
        | FixedVal


    /// Specifies how to generate catalytic rates.
    /// ByIndividualCatalyst - enforces thermodynamic constraint on each catalyst.
    /// ByEnantiomerPairs - enforces thermodynamic constraint on a pair of enantiomer catalysts.
    type CatalyticRateGenerationType =
        | ByIndividualCatalyst of CatRatesSimGenType
        | ByEnantiomerPairs of CatRatesSimGenType

        member this.catRatesSimGenType = match this with | ByIndividualCatalyst c | ByEnantiomerPairs c -> c


    type RateData =
        {
            forwardRate : ReactionRate option
            backwardRate : ReactionRate option
        }

        override r.ToString() = sprintf "{ f: %A; b: %A }" r.forwardRate r.backwardRate


    let bind f xOpt =
        match xOpt with
        | Some x -> f x
        | _ -> { forwardRate = None; backwardRate = None }


    type ReactionRateData<'R> =
        {
            reaction : 'R
            rateData : RateData
        }


    type RateGeneratorInfo<'A, 'B> =
        {
            a : array<'A>
            b : array<'B>
            reactionName : ReactionName
            successNumberType : SuccessNumberType
        }


    type RelatedReactions<'R> =
        {
            primary : RateData
            similar : list<ReactionRateData<'R>>
        }


    let getRatesWithSimilar (fo, rf) (bo, rb) s =
        let g so ro =
            match so, ro with
            | Some s, Some r -> s * r |> ReactionRate |> Some
            | _ -> None

        {
            primary = { forwardRate = g fo rf; backwardRate = g bo rb }
            similar = s
        }


    let getRates (fo, rf) (bo, rb) = getRatesWithSimilar (fo, rf) (bo, rb) []
    let getForwardRates (fo, rf) = getRates (fo, rf) (None, None)


    type CatRatesEeParam =
        {
            rateMultiplierDistr : RateMultiplierDistribution
            eeForwardDistribution : EeDistribution option
            eeBackwardDistribution : EeDistribution option
        }

        static member defaultValue =
            {
                rateMultiplierDistr = NoneRateMult
                eeForwardDistribution = None
                eeBackwardDistribution = None
            }


    type CatRatesInfo<'R, 'C, 'RC> =
        {
            reaction : 'R
            catalyst : 'C
            getCatEnantiomer : 'C -> 'C
            catReactionCreator : ('R * 'C) -> 'RC
            getBaseRates : 'R -> RateData // Get rates of base (not catalyzed) reaction.
            eeParams : CatRatesEeParam
            rateGenerationType : RateGenerationType
            rnd : RandomValueGetter
        }


    /// Thermodynamic considerations require that the equilibrium does not change in the presence of catalyst.
    /// That requires a racemic mixture of both chiral catalysts (because only a racemic mixture is in the equilibrium state).
    /// Therefore, if sf and sb are forward and backward rates of not catalyzed reaction, then
    /// total forward and backward multipliers due to racemic mixture of catalysts must be equal:
    ///
    /// kf + kfe = kb + kbe, where
    ///     kf -  is forward  multiplier for a catalyst C
    ///     kfe - is forward  multiplier for a catalyst E(C) - enantiomer of C
    ///     kb -  is backward multiplier for a catalyst C
    ///     kbe - is backward multiplier for a catalyst E(C)
    ///
    /// Setting eeParams.eeBackwardDistribution = None imposes more stringent constraint that
    /// forward and backward rate multipliers must be the same for each catalyst independently from
    /// its enantiomer. This seems to be more correct.
    let calculateCatRates<'R, 'C, 'RC> (i : CatRatesInfo<'R, 'C, 'RC>) =
        let re = (i.reaction, i.getCatEnantiomer i.catalyst) |> i.catReactionCreator

        let rf, rb, rfe, rbe =
            let k =
                match i.rateGenerationType with
                | RandomChoice -> i.eeParams.rateMultiplierDistr.nextDouble i.rnd

            match k, i.eeParams.eeForwardDistribution with
            | Some k0, Some df ->
                let s0 = i.getBaseRates i.reaction
                let fEe = df.nextDouble i.rnd

                let bEe =
                    match i.eeParams.eeBackwardDistribution with
                    | Some d -> d.nextDouble i.rnd
                    | None -> fEe

                let kf = k0 * (1.0 + fEe)
                let kfe = k0 * (1.0 - fEe)
                let kb = k0 * (1.0 + bEe)
                let kbe = k0 * (1.0 - bEe)

                let (rf, rfe) =
                    match s0.forwardRate with
                    | Some (ReactionRate sf) -> (kf * sf |> ReactionRate |> Some, kfe * sf |> ReactionRate |> Some)
                    | None -> (None, None)

                let (rb, rbe) =
                    match s0.backwardRate with
                    | Some (ReactionRate sb) -> (kb * sb |> ReactionRate |> Some, kbe * sb |> ReactionRate |> Some)
                    | None -> (None, None)

                (rf, rb, rfe, rbe)
            | _ -> (None, None, None, None)

        {
            primary = { forwardRate = rf; backwardRate = rb }
            similar = [ { reaction = re; rateData = { forwardRate = rfe; backwardRate = rbe } } ]
        }


    type CatRatesSimGeneration =
        | DistributionBased of Distribution
        | FixedValue of Distribution


    type CatRatesSimilarityParam =
        {
            catRatesSimGeneration : CatRatesSimGeneration
            getRateMultiplierDistr : RateMultiplierDistributionGetter
            getForwardEeDistr : EeDistributionGetter
            getBackwardEeDistr : EeDistributionGetter
        }


    type EnCatRatesInfo<'R, 'C, 'S, 'RCS> =
        {
            reaction : 'R
            catalyst : 'C
            energySource : 'S
            getCatEnantiomer : 'C -> 'C
            getEnergySourceEnantiomer : 'S -> 'S
            enCatReactionCreator : ('R * 'C * 'S) -> 'RCS
            getBaseRates : 'R -> RateData // Get rates of base (not catalyzed) reaction.
            eeParams : CatRatesEeParam
            rateGenerationType : RateGenerationType
            rnd : RandomValueGetter
        }

    let calculateEnCatRates<'R, 'C, 'S, 'RCS> (i : EnCatRatesInfo<'R, 'C, 'S, 'RCS>) : RelatedReactions<'RCS> =
        0

