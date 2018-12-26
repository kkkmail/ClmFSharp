namespace Clm
open System

/// The distributions that we need fall into the following categories:
///     1. EE distributions. They must produce values on (-1, 1) and usually have mean of 0.
///     2. Unconditional rate distributions. They must produce values on (0, infinity) with mean of 1.
//         This distribution is usually skewed toward 0.
///     3. Conditional rate distributions (or just rate distribution).
///        They must produce values on (0, infinity) with mean of 1.
///        This distribution produces value near mean.
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

        member distr.createScaled newScale creator = (distr.nextSeed(), { distr.distributionParams with scale = newScale }) |> creator
        member distr.createShifted newShift creator = (distr.nextSeed(), { distr.distributionParams with shift = newShift }) |> creator
        member distr.createThresholded newThreshold creator = (distr.nextSeed(), { distr.distributionParams with threshold = newThreshold }) |> creator


    /// Generates only 0 for default parameters.
    type DeltaDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase (seed, p, fun _ -> 0.0)

        static member create seed threshold scale shift = DeltaDistribution (seed, { threshold = threshold; scale = scale; shift = shift } )
        member distr.scaled newScale = distr.createScaled newScale DeltaDistribution
        member distr.shifted newShift = distr.createShifted newShift DeltaDistribution
        member distr.thresholded newThreshold = distr.createThresholded newThreshold DeltaDistribution


    ///// Generates only 1 for default parameters.
    //type DeltaPositiveDistribution (seed : int, p : DistributionParams) = 
    //    inherit DistributionBase (seed, p, fun _ -> 1.0)

    //    static member create seed threshold scale shift = DeltaPositiveDistribution (seed, { threshold = threshold; scale = scale; shift = shift } )
    //    member distr.scaled newScale = distr.createScaled newScale DeltaPositiveDistribution
    //    member distr.shifted newShift = distr.createShifted newShift DeltaPositiveDistribution
    //    member distr.thresholded newThreshold = distr.createThresholded newThreshold DeltaPositiveDistribution


    /// Generates values on (-1, 1) for default parameters.
    type UniformDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase (seed, p, fun r -> 2.0 * (r.NextDouble() - 1.0))

        new (seed : int) = UniformDistribution(seed, DistributionParams.defaultValue)

        static member create seed threshold scale shift = new UniformDistribution (seed, { threshold = threshold; scale = scale; shift = shift } )
        member distr.scaled newScale = distr.createScaled newScale UniformDistribution
        member distr.shifted newShift = distr.createShifted newShift UniformDistribution
        member distr.thresholded newThreshold = distr.createThresholded newThreshold UniformDistribution


    ///// Generates values on (0, 2) for default parameters.
    //type UniformPositiveDistribution (seed : int, p : DistributionParams) = 
    //    inherit DistributionBase (seed, p, fun r -> 2.0 * (r.NextDouble()))

    //    new (seed : int) = UniformPositiveDistribution(seed, DistributionParams.defaultValue)

    //    static member create seed threshold scale shift = new UniformPositiveDistribution (seed, { threshold = threshold; scale = scale; shift = shift } )
    //    member distr.scaled newScale = distr.createScaled newScale UniformPositiveDistribution
    //    member distr.shifted newShift = distr.createShifted newShift UniformPositiveDistribution
    //    member distr.thresholded newThreshold = distr.createThresholded newThreshold UniformPositiveDistribution


    /// Generates values on (0, 3) with mean 1 for default parameters.
    type TriangularDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase(seed, p, fun r -> 3.0 * (1.0 - sqrt(1.0 - r.NextDouble())))

        static member create seed threshold scale shift = new TriangularDistribution (seed, { threshold = threshold; scale = scale; shift = shift } )
        member distr.scaled newScale = distr.createScaled newScale TriangularDistribution
        member distr.shifted newShift = distr.createShifted newShift TriangularDistribution
        member distr.thresholded newThreshold = distr.createThresholded newThreshold TriangularDistribution


    let toSymmetricTriangular d = 
        if d < 0.5 then -1.0 + sqrt(2.0 * d)
        else 1.0 - sqrt(2.0 * (1.0 - d))


    let toSymmetricTriangularFromZeroToTwo d = 
        if d < 0.5 then sqrt(2.0 * d)
        else 2.0 - sqrt(2.0 * (1.0 - d))


    /// Generates values on (-1, 1) with max / mean at 0 for default parameters.
    type SymmetricTriangularDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase(seed, p, fun r -> r.NextDouble() |> toSymmetricTriangular)

        static member create seed threshold scale shift = new SymmetricTriangularDistribution (seed, { threshold = threshold; scale = scale; shift = shift } )
        member distr.scaled newScale = distr.createScaled newScale SymmetricTriangularDistribution
        member distr.shifted newShift = distr.createShifted newShift SymmetricTriangularDistribution
        member distr.thresholded newThreshold = distr.createThresholded newThreshold SymmetricTriangularDistribution


    ///// Generates values on (0, 2) with max / mean at 1 for default parameters.
    //type SymmetricPositiveTriangularDistribution (seed : int, p : DistributionParams) = 
    //    inherit DistributionBase(seed, p, fun r -> r.NextDouble() |> toSymmetricTriangularFromZeroToTwo)

    //    static member create seed threshold scale shift = new SymmetricPositiveTriangularDistribution (seed, { threshold = threshold; scale = scale; shift = shift } )
    //    member distr.scaled newScale = distr.createScaled newScale SymmetricPositiveTriangularDistribution
    //    member distr.shifted newShift = distr.createShifted newShift SymmetricPositiveTriangularDistribution
    //    member distr.thresholded newThreshold = distr.createThresholded newThreshold SymmetricPositiveTriangularDistribution


    type Distribution =
        | Delta of DeltaDistribution
        //| DeltaPositive of DeltaPositiveDistribution
        | Uniform of UniformDistribution
        //| UniformPositive of UniformPositiveDistribution
        | Triangular of TriangularDistribution
        | SymmetricTriangular of SymmetricTriangularDistribution
        //| SymmetricPositiveTriangular of SymmetricPositiveTriangularDistribution

        member this.nextDouble = 
            match this with
            | Delta d -> d.nextDouble
            //| DeltaPositive d -> d.nextDouble
            | Uniform d -> d.nextDouble
            //| UniformPositive d -> d.nextDouble
            | Triangular d -> d.nextDouble
            | SymmetricTriangular d -> d.nextDouble
            //| SymmetricPositiveTriangular d -> d.nextDouble

        member this.nextDoubleOpt = 
            match this with
            | Delta d -> d.nextDoubleOpt
            //| DeltaPositive d -> d.nextDoubleOpt
            | Uniform d -> d.nextDoubleOpt
            //| UniformPositive d -> d.nextDoubleOpt
            | Triangular d -> d.nextDoubleOpt
            | SymmetricTriangular d -> d.nextDoubleOpt
            //| SymmetricPositiveTriangular d -> d.nextDoubleOpt

        member this.isDefined =
            match this with
            | Delta d -> d.isDefined
            //| DeltaPositive d -> d.isDefined
            | Uniform d -> d.isDefined
            //| UniformPositive d -> d.isDefined
            | Triangular d -> d.isDefined
            | SymmetricTriangular d -> d.isDefined
            //| SymmetricPositiveTriangular d -> d.isDefined

        member this.nextSeed() = 
            match this with
            | Delta d -> d.nextSeed()
            //| DeltaPositive d -> d.nextSeed()
            | Uniform d -> d.nextSeed()
            //| UniformPositive d -> d.nextSeed()
            | Triangular d -> d.nextSeed()
            | SymmetricTriangular d -> d.nextSeed()
            //| SymmetricPositiveTriangular d -> d.nextSeed()

        member this.scaled newScale =
            match this with
            | Delta d -> d.scaled newScale |> Delta
            //| DeltaPositive d -> d.scaled newScale |> DeltaPositive
            | Uniform d -> d.scaled newScale |> Uniform
            //| UniformPositive d -> d.scaled newScale |> UniformPositive
            | Triangular d -> d.scaled newScale |> Triangular
            | SymmetricTriangular d ->  d.scaled newScale |> SymmetricTriangular
            //| SymmetricPositiveTriangular d -> d.scaled newScale |> SymmetricPositiveTriangular

        member this.shifted newShift =
            match this with
            | Delta d -> d.shifted newShift |> Delta
            //| DeltaPositive d -> d.shifted newShift |> DeltaPositive
            | Uniform d -> d.shifted newShift |> Uniform
            //| UniformPositive d -> d.shifted newShift |> UniformPositive
            | Triangular d -> d.shifted newShift |> Triangular
            | SymmetricTriangular d -> d.shifted newShift |> SymmetricTriangular
            //| SymmetricPositiveTriangular d -> d.shifted newShift |> SymmetricPositiveTriangular

        member this.thresholded newThreshold =
            match this with
            | Delta d -> d.thresholded newThreshold |> Delta
            //| DeltaPositive d -> d.thresholded newThreshold |> DeltaPositive
            | Uniform d -> d.thresholded newThreshold |> Uniform
            //| UniformPositive d -> d.thresholded newThreshold |> UniformPositive
            | Triangular d -> d.thresholded newThreshold |> Triangular
            | SymmetricTriangular d -> d.thresholded newThreshold |> SymmetricTriangular
            //| SymmetricPositiveTriangular d -> d.thresholded newThreshold |> SymmetricPositiveTriangular


    /// EE distributiolns. They are specially formatted distributions to return values only between (-1 and 1).
    type EeDistribution = 
        | DeltaEe of DeltaDistribution
        | UniformEe of UniformDistribution
        | SymmetricTriangularEe of SymmetricTriangularDistribution

        member eed.nextDouble() : double = 
            let v = 
                match eed with 
                    | DeltaEe d -> d.nextDouble()
                    | UniformEe d -> d.nextDouble()
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


    /// Distribution of rate multipliers for catalytic reactions.
    type RateMultiplierDistribution = 
        | DeltaRateMult of DeltaDistribution
        | SymmetricPositiveTriangularRateMult of SymmetricPositiveTriangularDistribution
        | NoneRateMult

        static member private normalize d = d |> Option.bind (fun e -> max e 0.0 |> Some)

        member this.nextDoubleOpt() = 
            match this with 
            | DeltaRateMult d -> d.nextDoubleOpt() |> RateMultiplierDistribution.normalize
            | SymmetricPositiveTriangularRateMult d -> d.nextDoubleOpt() |> RateMultiplierDistribution.normalize
            | NoneRateMult -> None

        member this.thresholded newThreshold = 
            match this with 
            | DeltaRateMult d -> d.thresholded newThreshold |> DeltaRateMult
            | SymmetricPositiveTriangularRateMult d -> d.thresholded newThreshold |> SymmetricPositiveTriangularRateMult
            | NoneRateMult -> NoneRateMult

        member this.scaled newScale = 
            match this with 
            | DeltaRateMult d -> d.scaled newScale |> DeltaRateMult
            | SymmetricPositiveTriangularRateMult d -> d.scaled newScale |> SymmetricPositiveTriangularRateMult
            | NoneRateMult -> NoneRateMult

        member this.shifted newShift = 
            match this with 
            | DeltaRateMult d -> d.shifted newShift |> DeltaRateMult
            | SymmetricPositiveTriangularRateMult d -> d.shifted newShift |> SymmetricPositiveTriangularRateMult
            | NoneRateMult -> NoneRateMult

        static member createDefault (seeder : unit -> int) rate = 
            SymmetricPositiveTriangularDistribution(seeder(), { threshold = None; scale = Some rate; shift = None }) |> SymmetricPositiveTriangularRateMult

        static member private createDelta (seeder : unit -> int) rate = 
            DeltaDistribution (seeder(), { threshold = None; scale = None; shift = Some rate }) |> DeltaRateMult


    type RateMultiplierDistributionGetter =
        | DefaultRateMultiplierDistributionGetter
        | DeltaRateMultiplierDistributionGetter

        member this.getDistr (d : RateMultiplierDistribution) threshold rate = 
            match this with
            | DefaultRateMultiplierDistributionGetter -> (d.thresholded threshold).scaled (Some rate)
            | DeltaRateMultiplierDistributionGetter -> failwith ""
