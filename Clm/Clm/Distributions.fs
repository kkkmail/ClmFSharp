namespace Clm
open System

module Distributions = 

    type ReactionRate = 
        | ReactionRate of double


    type DistributionParams = 
        {
            threshold : double option
            scale : double option
            shift : double option
        }

        static member defaultValue = 
            {
                threshold = None
                scale = None
                shift = None
            }


    /// First scale, then shift. This is more convenient here than the other way around.
    [<AbstractClass>]
    type DistributionBase(seed : int, p : DistributionParams, d : Random -> double) = 
        let rnd = new Random(seed)

        let isDefinedImpl() = 
            match p.threshold with
            | Some t -> if rnd.NextDouble() < t then true else false
            | None -> true

        let nextDoubleImpl() = 
            let v = 
                d(rnd) *
                match p.scale with 
                | Some s -> s
                | None -> 1.0

            v + 
            match p.shift with 
            | Some s -> s
            | None -> 0.0

        member __.seedValue = seed
        member __.distributionParams = p
        member __.nextDouble = nextDoubleImpl
        member __.nextSeed() = rnd.Next()

        member __.nextDoubleOpt() = 
            match isDefinedImpl() with 
            | true -> nextDoubleImpl() |> Some
            | false -> None

        member __.isDefined = isDefinedImpl


    /// Produces only 0 with default parameters.
    type DeltaDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase (seed, p, fun _ -> 0.0)

        static member create seed threshold scale shift = 
            new DeltaDistribution (seed, { threshold = threshold; scale = scale; shift = shift } )


    type UniformDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase (seed, p, fun r -> r.NextDouble())

        new (seed : int) = UniformDistribution(seed, DistributionParams.defaultValue)

        static member create seed threshold scale shift = 
            new UniformDistribution (seed, { threshold = threshold; scale = scale; shift = shift } )


    type TriangularDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase(seed, p, fun r -> 1.0 - sqrt(1.0 - r.NextDouble()))

        static member create seed threshold scale shift = 
            new TriangularDistribution (seed, { threshold = threshold; scale = scale; shift = shift } )


    let toSymmetricTriangular d = 
        if d < 0.5 then -1.0 + sqrt(2.0 * d)
        else 1.0 - sqrt(2.0 * (1.0 - d))


    let toSymmetricTriangularFromZeroToTwo d = 
        if d < 0.5 then sqrt(2.0 * d)
        else 2.0 - sqrt(2.0 * (1.0 - d))


    /// Generates values on (-1, 1) with max / mean at 0 with default parameters.
    type SymmetricTriangularDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase(seed, p, fun r -> r.NextDouble() |> toSymmetricTriangular)

        static member create seed threshold scale shift = 
            new SymmetricTriangularDistribution (seed, { threshold = threshold; scale = scale; shift = shift } )


    /// Generates values on (0, 2) with max / mean at 1 with default parameters.
    type SymmetricPositiveTriangularDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase(seed, p, fun r -> r.NextDouble() |> toSymmetricTriangularFromZeroToTwo)

        static member create seed threshold scale shift = 
            new SymmetricPositiveTriangularDistribution (seed, { threshold = threshold; scale = scale; shift = shift } )


    type Distribution =
        | Delta of DeltaDistribution
        | Uniform of UniformDistribution
        | Triangular of TriangularDistribution
        | SymmetricTriangular of SymmetricTriangularDistribution
        | SymmetricPositiveTriangular of SymmetricPositiveTriangularDistribution

        member this.nextDouble = 
            match this with
            | Delta d -> d.nextDouble
            | Uniform d -> d.nextDouble
            | Triangular d -> d.nextDouble
            | SymmetricTriangular d -> d.nextDouble
            | SymmetricPositiveTriangular d -> d.nextDouble

        member this.nextDoubleOpt = 
            match this with
            | Delta d -> d.nextDoubleOpt
            | Uniform d -> d.nextDoubleOpt
            | Triangular d -> d.nextDoubleOpt
            | SymmetricTriangular d -> d.nextDoubleOpt
            | SymmetricPositiveTriangular d -> d.nextDoubleOpt

        member this.isDefined =
            match this with
            | Delta d -> d.isDefined
            | Uniform d -> d.isDefined
            | Triangular d -> d.isDefined
            | SymmetricTriangular d -> d.isDefined
            | SymmetricPositiveTriangular d -> d.isDefined

        member this.nextSeed() = 
            match this with
            | Delta d -> d.nextSeed()
            | Uniform d -> d.nextSeed()
            | Triangular d -> d.nextSeed()
            | SymmetricTriangular d -> d.nextSeed()
            | SymmetricPositiveTriangular d -> d.nextSeed()

        member this.scaled newScale =
            match this with
            | Delta d -> DeltaDistribution (d.nextSeed(), { d.distributionParams with scale = newScale }) |> Delta
            | Uniform d -> UniformDistribution (d.nextSeed(), { d.distributionParams with scale = newScale }) |> Uniform
            | Triangular d -> TriangularDistribution (d.nextSeed(), { d.distributionParams with scale = newScale }) |> Triangular
            | SymmetricTriangular d -> SymmetricTriangularDistribution (d.nextSeed(), { d.distributionParams with scale = newScale }) |> SymmetricTriangular
            | SymmetricPositiveTriangular d -> SymmetricPositiveTriangularDistribution (d.nextSeed(), { d.distributionParams with scale = newScale }) |> SymmetricPositiveTriangular

        member this.shifted newShift =
            match this with
            | Delta d -> DeltaDistribution (d.nextSeed(), { d.distributionParams with shift = newShift }) |> Delta
            | Uniform d -> UniformDistribution (d.nextSeed(), { d.distributionParams with shift = newShift }) |> Uniform
            | Triangular d -> TriangularDistribution (d.nextSeed(), { d.distributionParams with shift = newShift }) |> Triangular
            | SymmetricTriangular d -> SymmetricTriangularDistribution (d.nextSeed(), { d.distributionParams with shift = newShift }) |> SymmetricTriangular
            | SymmetricPositiveTriangular d -> SymmetricPositiveTriangularDistribution (d.nextSeed(), { d.distributionParams with shift = newShift }) |> SymmetricPositiveTriangular

        member this.thresholded newThreshold =
            match this with
            | Delta d -> DeltaDistribution (d.nextSeed(), { d.distributionParams with threshold = newThreshold }) |> Delta
            | Uniform d -> UniformDistribution (d.nextSeed(), { d.distributionParams with threshold = newThreshold }) |> Uniform
            | Triangular d -> TriangularDistribution (d.nextSeed(), { d.distributionParams with threshold = newThreshold }) |> Triangular
            | SymmetricTriangular d -> SymmetricTriangularDistribution (d.nextSeed(), { d.distributionParams with threshold = newThreshold }) |> SymmetricTriangular
            | SymmetricPositiveTriangular d -> SymmetricPositiveTriangularDistribution (d.nextSeed(), { d.distributionParams with threshold = newThreshold }) |> SymmetricPositiveTriangular


    /// Distribution of rate multipliers for catalytic reactions.
    type RateMultiplierDistribution = 
        | RateMultiplierDistr of Distribution
        | NoneDistr

        member this.nextDoubleOpt() = 
            match this with 
            | RateMultiplierDistr d -> d.nextDoubleOpt() |> Option.bind (fun e -> max e 0.0 |> Some)
            | NoneDistr -> None

        member this.thresholded newThreshold = 
            match this with 
            | RateMultiplierDistr d -> d.thresholded newThreshold |> RateMultiplierDistr
            | NoneDistr -> NoneDistr

        member this.scaled newScale = 
            match this with 
            | RateMultiplierDistr d -> d.scaled newScale |> RateMultiplierDistr
            | NoneDistr -> NoneDistr

        member this.shifted newShift = 
            match this with 
            | RateMultiplierDistr d -> d.shifted newShift |> RateMultiplierDistr
            | NoneDistr -> NoneDistr


    /// EE distributiolns. They are specially formatted distributions to return values only between (-1 and 1).
    type EeDistribution = 
        | DeltaEe of DeltaDistribution
        | SymmetricTriangularEe of SymmetricTriangularDistribution

        member eed.nextDouble() : double = 
            let v = 
                match eed with 
                    | DeltaEe d -> d.nextDouble()
                    | SymmetricTriangularEe d -> d.nextDouble()

            max (min v 1.0) (-1.0)

        static member createDefault (seeder : unit -> int) = 
            SymmetricTriangularDistribution(seeder(), { threshold = None; scale = None; shift = None }) |> SymmetricTriangularEe

        static member private getMeanAndWidth mean =
            match mean with
            | x when x <= -1.0 -> -1.0, None
            | x when -1.0 < x && x < 1.0 -> x, min (1.0 - x) (x + 1.0) |> Some
            | x when x >= 1.0 -> 1.0, None
            | _ -> 0.0, Some 1.0

        static member private createCenteredDelta (seeder : unit -> int) mean = 
            let m, _ = EeDistribution.getMeanAndWidth mean
            DeltaDistribution (seeder(), { threshold = None; scale = None; shift = Some m }) |> DeltaEe

        static member private createCentered (seeder : unit -> int) mean = 
            let m, w = EeDistribution.getMeanAndWidth mean

            match w with
            | Some s -> SymmetricTriangularDistribution(seeder(), { threshold = None; scale = Some s; shift = Some m }) |> SymmetricTriangularEe
            | None -> EeDistribution.createCenteredDelta seeder mean

        static member getDefaultEeDistrOpt (seeder : unit -> int) (rate : ReactionRate option) (rateEnant : ReactionRate option) = 
            match rate, rateEnant with 
            | Some (ReactionRate r), Some (ReactionRate re) -> (r - re) / (r + re) |> EeDistribution.createCentered seeder |> Some
            | _ -> None

        static member getDeltaEeDistrOpt (seeder : unit -> int) (rate : ReactionRate option) (rateEnant : ReactionRate option) = 
            match rate, rateEnant with 
            | Some (ReactionRate r), Some (ReactionRate re) -> (r - re) / (r + re) |> EeDistribution.createCenteredDelta seeder |> Some
            | _ -> None


    type EeDistributionGetter = 
        | DefaultEeDistributionGetter
        | DeltaEeDistributionGetter
        | NoneGetter

        member ee.getDistr = 
            match ee with 
            | DefaultEeDistributionGetter -> EeDistribution.getDefaultEeDistrOpt
            | DeltaEeDistributionGetter -> EeDistribution.getDeltaEeDistrOpt
            | NoneGetter -> (fun _ _ _ -> None)


    type RateMultiplierDistributionGetter =
        | DefaultRateMultiplierDistributionGetter
        | DeltaRateMultiplierDistributionGetter

        member this.getDistr (d : RateMultiplierDistribution) threshold rate = 
            match this with
            | DefaultRateMultiplierDistributionGetter -> (d.thresholded threshold).scaled (Some rate)
            | DeltaRateMultiplierDistributionGetter -> failwith ""
