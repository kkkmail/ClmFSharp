namespace Clm
open System
open ClmSys.GeneralData

/// The distributions that we need fall into the following categories:
///     1. EE distributions. They must produce values on (-1, 1) and usually have mean of 0.
///     2. Unconditional rate distributions (RateDistribution). They must produce values on (0, infinity) with mean of 1.
//         This distribution is usually skewed toward 0.
///     3. Conditional rate distributions (RateDistribution).
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


    /// Generates only -1 and 1 with equal probability for default parameters.
    type BiDeltaDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase (seed, p, fun r -> if r.NextDouble() < 0.5 then -1.0 else 1.0)

        static member create seed threshold scale shift = BiDeltaDistribution (seed, { threshold = threshold; scale = scale; shift = shift } )
        member distr.scaled newScale = distr.createScaled newScale BiDeltaDistribution
        member distr.shifted newShift = distr.createShifted newShift BiDeltaDistribution
        member distr.thresholded newThreshold = distr.createThresholded newThreshold BiDeltaDistribution


    /// Generates values on (-1, 1) for default parameters.
    type UniformDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase (seed, p, fun r -> 2.0 * (r.NextDouble() - 1.0))

        new (seed : int) = UniformDistribution(seed, DistributionParams.defaultValue)

        static member create seed threshold scale shift = new UniformDistribution (seed, { threshold = threshold; scale = scale; shift = shift } )
        member distr.scaled newScale = distr.createScaled newScale UniformDistribution
        member distr.shifted newShift = distr.createShifted newShift UniformDistribution
        member distr.thresholded newThreshold = distr.createThresholded newThreshold UniformDistribution


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


    /// Generates values on (-1, 1) with max / mean at 0 for default parameters.
    type SymmetricTriangularDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase(seed, p, fun r -> r.NextDouble() |> toSymmetricTriangular)

        static member create seed threshold scale shift = new SymmetricTriangularDistribution (seed, { threshold = threshold; scale = scale; shift = shift } )
        member distr.scaled newScale = distr.createScaled newScale SymmetricTriangularDistribution
        member distr.shifted newShift = distr.createShifted newShift SymmetricTriangularDistribution
        member distr.thresholded newThreshold = distr.createThresholded newThreshold SymmetricTriangularDistribution


    [<Literal>]
    let DistributionName = "Distribution"


    [<Literal>]
    let DeltaName = "Delta"


    [<Literal>]
    let BiDeltaName = "BiDelta"


    [<Literal>]
    let UniformName = "Uniform"


    [<Literal>]
    let TriangularName = "Triangular"


    [<Literal>]
    let SymmetricTriangularName = "SymmetricTriangular"


    [<CustomEquality>]
    [<CustomComparison>]
    type Distribution =
        | Delta of DeltaDistribution
        | BiDelta of BiDeltaDistribution
        | Uniform of UniformDistribution
        | Triangular of TriangularDistribution
        | SymmetricTriangular of SymmetricTriangularDistribution

        member this.nextDouble = 
            match this with
            | Delta d -> d.nextDouble
            | BiDelta d -> d.nextDouble
            | Uniform d -> d.nextDouble
            | Triangular d -> d.nextDouble
            | SymmetricTriangular d -> d.nextDouble

        member this.nextDoubleOpt = 
            match this with
            | Delta d -> d.nextDoubleOpt
            | BiDelta d -> d.nextDoubleOpt
            | Uniform d -> d.nextDoubleOpt
            | Triangular d -> d.nextDoubleOpt
            | SymmetricTriangular d -> d.nextDoubleOpt

        member this.isDefined =
            match this with
            | Delta d -> d.isDefined
            | BiDelta d -> d.isDefined
            | Uniform d -> d.isDefined
            | Triangular d -> d.isDefined
            | SymmetricTriangular d -> d.isDefined

        member this.nextSeed() = 
            match this with
            | Delta d -> d.nextSeed()
            | BiDelta d -> d.nextSeed()
            | Uniform d -> d.nextSeed()
            | Triangular d -> d.nextSeed()
            | SymmetricTriangular d -> d.nextSeed()

        member this.scaled newScale =
            match this with
            | Delta d -> d.scaled newScale |> Delta
            | BiDelta d -> d.scaled newScale |> BiDelta
            | Uniform d -> d.scaled newScale |> Uniform
            | Triangular d -> d.scaled newScale |> Triangular
            | SymmetricTriangular d ->  d.scaled newScale |> SymmetricTriangular

        member this.shifted newShift =
            match this with
            | Delta d -> d.shifted newShift |> Delta
            | BiDelta d -> d.shifted newShift |> BiDelta
            | Uniform d -> d.shifted newShift |> Uniform
            | Triangular d -> d.shifted newShift |> Triangular
            | SymmetricTriangular d -> d.shifted newShift |> SymmetricTriangular

        member this.thresholded newThreshold =
            match this with
            | Delta d -> d.thresholded newThreshold |> Delta
            | BiDelta d -> d.thresholded newThreshold |> BiDelta
            | Uniform d -> d.thresholded newThreshold |> Uniform
            | Triangular d -> d.thresholded newThreshold |> Triangular
            | SymmetricTriangular d -> d.thresholded newThreshold |> SymmetricTriangular

        member this.name =
            match this with
            | Delta _ -> DeltaName
            | BiDelta _ -> BiDeltaName
            | Uniform _ -> UniformName
            | Triangular _ -> TriangularName
            | SymmetricTriangular _ -> SymmetricTriangularName

        member this.distributionParams =
            match this with
            | Delta d -> d.distributionParams
            | BiDelta d -> d.distributionParams
            | Uniform d -> d.distributionParams
            | Triangular d -> d.distributionParams
            | SymmetricTriangular d -> d.distributionParams

        member this.seedValue =
            match this with
            | Delta d -> d.seedValue
            | BiDelta d -> d.seedValue
            | Uniform d -> d.seedValue
            | Triangular d -> d.seedValue
            | SymmetricTriangular d -> d.seedValue

        override this.Equals (o: obj) =
            match o with
            | :? Distribution as d -> this.distributionParams = d.distributionParams
            | _ -> false

        override this.GetHashCode() = hash (this.name, this.distributionParams)

        interface IEquatable<Distribution> with
            member this.Equals(that : Distribution) = this.Equals(that)

        interface IComparable with
            member this.CompareTo(thatObj) =
                match thatObj with
                | :? Distribution as that ->
                    compare (this.name, this.distributionParams) (that.name, that.distributionParams)
                | _ ->
                    raise <| ArgumentException("Can't compare instances of different types.")


    [<Literal>]
    let EeDistributionName = "EeDistribution"


    /// EE distributiolns. They are specially formatted distributions to return values only between (-1 and 1).
    type EeDistribution = 
        | EeDistribution of Distribution

        member eed.nextDouble() = 
            let (EeDistribution d) = eed
            max (min (d.nextDouble()) 1.0) (-1.0)

        member eed.name =
            match eed with
            | EeDistribution _ -> EeDistributionName

        static member createSymmetricTriangular (seeder : unit -> int) = 
            SymmetricTriangularDistribution(seeder(), { threshold = None; scale = None; shift = None }) |> SymmetricTriangular |> EeDistribution

        static member createBiDelta scale (seeder : unit -> int) = 
            BiDeltaDistribution(seeder(), { threshold = None; scale = scale; shift = None }) |> BiDelta |> EeDistribution

        static member private getMeanAndWidth mean =
            match mean with
            | x when x <= -1.0 -> -1.0, None
            | x when -1.0 < x && x < 1.0 -> x, min (1.0 - x) (x + 1.0) |> Some
            | x when x >= 1.0 -> 1.0, None
            | _ -> 0.0, Some 1.0

        static member private createCenteredDelta (seeder : unit -> int) mean = 
            let m, _ = EeDistribution.getMeanAndWidth mean
            DeltaDistribution (seeder(), { threshold = None; scale = None; shift = Some m }) |> Delta |> EeDistribution

        static member private createCentered (seeder : unit -> int) mean = 
            let m, w = EeDistribution.getMeanAndWidth mean

            match w with
            | Some s -> SymmetricTriangularDistribution(seeder(), { threshold = None; scale = Some s; shift = Some m }) |> SymmetricTriangular |> EeDistribution
            | None -> EeDistribution.createCenteredDelta seeder mean

        static member getDeltaEeDistrOpt (seeder : unit -> int) (rate : ReactionRate option) (rateEnant : ReactionRate option) = 
            match rate, rateEnant with 
            | Some (ReactionRate r), Some (ReactionRate re) -> (r - re) / (r + re) |> EeDistribution.createCenteredDelta seeder |> Some
            | _ -> None

        static member getCenteredEeDistrOpt (seeder : unit -> int) (rate : ReactionRate option) (rateEnant : ReactionRate option) = 
            match rate, rateEnant with 
            | Some (ReactionRate r), Some (ReactionRate re) -> (r - re) / (r + re) |> EeDistribution.createCentered seeder |> Some
            | _ -> None

        static member getDefaultEeDistrOpt = EeDistribution.getCenteredEeDistrOpt


    [<Literal>]
    let EeDistributionGetterName = "EeDistributionGetter"


    [<Literal>]
    let NoneEeGetterName = "NoneEeGetter"


    [<Literal>]
    let DeltaEeDistributionGetterName = "DeltaEeDistributionGetter"


    [<Literal>]
    let CenteredEeDistributionGetterName = "CenteredEeDistributionGetter"


    type EeDistributionGetter = 
        | NoneEeGetter
        | DeltaEeDistributionGetter
        | CenteredEeDistributionGetter

        member ee.getDistr = 
            match ee with 
            | NoneEeGetter -> (fun _ _ _ -> None)
            | DeltaEeDistributionGetter -> EeDistribution.getDeltaEeDistrOpt
            | CenteredEeDistributionGetter -> EeDistribution.getCenteredEeDistrOpt

        member ee.name = 
            match ee with 
            | NoneEeGetter -> "NoneEeGetter"
            | DeltaEeDistributionGetter -> "DeltaEeDistributionGetter"
            | CenteredEeDistributionGetter -> "CenteredEeDistributionGetter"


    [<Literal>]
    let RateMultiplierDistributionName = "RateMultiplierDistribution"


    [<Literal>]
    let NoneRateMultName = "NoneRateMult"


    [<Literal>]
    let RateMultDistrName = "RateMultDistr"


    /// Distribution of rate multipliers for catalytic reactions.
    type RateMultiplierDistribution = 
        | NoneRateMult
        | RateMultDistr of Distribution

        static member private normalize d = d |> Option.bind (fun e -> max e 0.0 |> Some)

        member this.nextDoubleOpt() = 
            match this with 
            | NoneRateMult -> None
            | RateMultDistr d -> d.nextDoubleOpt() |> RateMultiplierDistribution.normalize

        member this.name =
            match this with 
            | NoneRateMult -> NoneRateMultName
            | RateMultDistr _ -> RateMultDistrName

        static member createNone = NoneRateMult

        static member createDelta (seeder : unit -> int) threshold rate = 
            DeltaDistribution (seeder(), { threshold = threshold; scale = None; shift = Some rate }) |> Delta |> RateMultDistr

        static member createTriangular (seeder : unit -> int) threshold rate = 
            TriangularDistribution(seeder(), { threshold = threshold; scale = Some rate; shift = None }) |> Triangular |> RateMultDistr

        static member createSymmetricTriangular (seeder : unit -> int) threshold rate = 
            SymmetricTriangularDistribution(seeder(), { threshold = threshold; scale = Some rate; shift = Some rate }) |> SymmetricTriangular |> RateMultDistr


    [<Literal>]
    let RateMultiplierDistributionGetterName = "RateMultiplierDistributionGetter"


    [<Literal>]
    let NoneRateMultDistrGetterName = "NoneRateMultDistrGetter"


    [<Literal>]
    let DeltaRateMultDistrGetterName = "DeltaRateMultDistrGetter"


    [<Literal>]
    let TriangularRateMultDistrGetterName = "TriangularRateMultDistrGetter"


    [<Literal>]
    let SymmetricTriangularRateMultDistrGetterName = "SymmetricTriangularRateMultDistrGetter"


    type RateMultiplierDistributionGetter =
        | NoneRateMultDistrGetter
        | DeltaRateMultDistrGetter
        | TriangularRateMultDistrGetter
        | SymmetricTriangularRateMultDistrGetter

        member this.getDistr seeder threshold rate =
            match this with
            | NoneRateMultDistrGetter -> RateMultiplierDistribution.createNone
            | DeltaRateMultDistrGetter -> RateMultiplierDistribution.createDelta seeder threshold rate
            | TriangularRateMultDistrGetter -> RateMultiplierDistribution.createTriangular seeder threshold rate
            | SymmetricTriangularRateMultDistrGetter -> RateMultiplierDistribution.createSymmetricTriangular seeder threshold rate

        member this.name =
            match this with
            | NoneRateMultDistrGetter -> NoneRateMultDistrGetterName
            | DeltaRateMultDistrGetter -> DeltaRateMultDistrGetterName
            | TriangularRateMultDistrGetter -> TriangularRateMultDistrGetterName
            | SymmetricTriangularRateMultDistrGetter -> SymmetricTriangularRateMultDistrGetterName
