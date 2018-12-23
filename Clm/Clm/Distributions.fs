namespace Clm
open System

module Distributions = 

    type ReactionRate = 
        | ReactionRate of double


    /// First scale, then shift. This is more convenient here than the other way around.
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


    [<AbstractClass>]
    type DistributionBase(seed : int, p : DistributionParams, d : Random -> double) = 
        let rnd = new Random(seed)
        let rndBool = new Random(rnd.Next())

        let isDefinedImpl() = 
            match p.threshold with
            | Some t -> if rndBool.NextDouble() < t then true else false
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

        let nextDoubleFromZeroToOneImpl() = rnd.NextDouble()

        member __.seedValue = seed
        member __.distributionParams = p
        member __.nextDouble = nextDoubleImpl
        member __.nextDoubleFromZeroToOne = nextDoubleFromZeroToOneImpl
        member __.nextSeed() = rnd.Next()

        member __.nextDoubleOpt() = 
            match isDefinedImpl() with 
            | true -> nextDoubleImpl() |> Some
            | false -> None

        member __.isDefined = isDefinedImpl


    type DeltaDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase (seed, p, fun _ -> 1.0)


    type UniformDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase (seed, p, fun r -> r.NextDouble())

        new (seed : int) = UniformDistribution(seed, DistributionParams.defaultValue)


    type TriangularDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase(seed, p, fun r -> 1.0 - sqrt(1.0 - r.NextDouble()))


    let toSymmetricTriangular d = 
        if d < 0.5 then -1.0 + sqrt(2.0 * d)
        else 1.0 - sqrt(2.0 * (1.0 - d))


    /// Genetates values on (-1 + p.shift, 1 + p.shift).
    type SymmetricTriangularDistribution (seed : int, p : DistributionParams) = 
        inherit DistributionBase(seed, p, fun r -> r.NextDouble() |> toSymmetricTriangular)


    type Distribution =
        | Delta of DeltaDistribution
        | Uniform of UniformDistribution
        | Triangular of TriangularDistribution
        | SymmetricTriangular of SymmetricTriangularDistribution

        member this.nextDouble = 
            match this with
            | Delta d -> d.nextDouble
            | Uniform d -> d.nextDouble
            | Triangular d -> d.nextDouble
            | SymmetricTriangular d -> d.nextDouble

        member this.nextDoubleOpt = 
            match this with
            | Delta d -> d.nextDoubleOpt
            | Uniform d -> d.nextDoubleOpt
            | Triangular d -> d.nextDoubleOpt
            | SymmetricTriangular d -> d.nextDoubleOpt

        member this.nextDoubleFromZeroToOne =
            match this with
            | Delta d -> d.nextDoubleFromZeroToOne
            | Uniform d -> d.nextDoubleFromZeroToOne
            | Triangular d -> d.nextDoubleFromZeroToOne
            | SymmetricTriangular d -> d.nextDoubleFromZeroToOne

        member this.isDefined =
            match this with
            | Delta d -> d.isDefined
            | Uniform d -> d.isDefined
            | Triangular d -> d.isDefined
            | SymmetricTriangular d -> d.isDefined

        member this.nextSeed() = 
            match this with
            | Delta d -> d.nextSeed()
            | Uniform d -> d.nextSeed()
            | Triangular d -> d.nextSeed()
            | SymmetricTriangular d -> d.nextSeed()

        member this.scaled newScale =
            match this with
            | Delta d -> DeltaDistribution (d.nextSeed(), { d.distributionParams with scale = newScale }) |> Delta
            | Uniform d -> UniformDistribution (d.nextSeed(), { d.distributionParams with scale = newScale }) |> Uniform
            | Triangular d -> TriangularDistribution (d.nextSeed(), { d.distributionParams with scale = newScale }) |> Triangular
            | SymmetricTriangular d -> SymmetricTriangularDistribution (d.nextSeed(), { d.distributionParams with scale = newScale }) |> SymmetricTriangular

        member this.shifted newshift =
            match this with
            | Delta d -> DeltaDistribution (d.nextSeed(), { d.distributionParams with shift = newshift }) |> Delta
            | Uniform d -> UniformDistribution (d.nextSeed(), { d.distributionParams with shift = newshift }) |> Uniform
            | Triangular d -> TriangularDistribution (d.nextSeed(), { d.distributionParams with shift = newshift }) |> Triangular
            | SymmetricTriangular d -> SymmetricTriangularDistribution (d.nextSeed(), { d.distributionParams with shift = newshift }) |> SymmetricTriangular


    /// Distribution of rate multipliers for catalytic reactions.
    type RateMultiplierDistribution = 
        | RateMultiplierDistribution of Distribution

        member this.nextDoubleOpt() = 
            let (RateMultiplierDistribution d) = this
            d.nextDoubleOpt() |> Option.bind (fun e -> max e 0.0 |> Some)

        member this.scaled newScale = 
            let (RateMultiplierDistribution d) = this
            d.scaled newScale

        member this.shifted newScale = 
            let (RateMultiplierDistribution d) = this
            d.shifted newScale


    /// Specially formatted distributions to return values only between (-1 and 1).
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

        static member createCentered (seeder : unit -> int) mean = 
            let m, w = 
                match mean with 
                | x when x <= -1.0 -> -1.0, None
                | x when -1.0 < x && x < 1.0 -> x, min (1.0 - x) (x + 1.0) |> Some
                | x when x >= 1.0 -> 1.0, None
                | _ -> 0.0, Some 1.0

            match w with 
            | Some s -> SymmetricTriangularDistribution(seeder(), { threshold = None; scale = Some s; shift = Some m }) |> SymmetricTriangularEe
            | None -> DeltaDistribution (seeder(), { threshold = None; scale = None; shift = Some m }) |> DeltaEe

        static member getDefaultEeDistr (seeder : unit -> int) (rate : ReactionRate option) (rateEnant : ReactionRate option) = 
            match rate, rateEnant with 
            | Some (ReactionRate r), Some (ReactionRate re) -> 
                (r - re) / (r + re) |> EeDistribution.createCentered seeder |> Some
            | _ -> None


    type EeDistributionGetter = 
        | DefaultEeDistributionGetter

        member ee.getDistr = 
            match ee with 
            | DefaultEeDistributionGetter -> EeDistribution.getDefaultEeDistr


    ///// Specially formatted distributions to return values above 0 and with max / mean at around 1.
    //type SimDistribution = 
    //    | DeltaSim of DeltaDistribution
    //    | SymmetricTriangularSim of SymmetricTriangularDistribution

    //    member sym.nextDouble() : double = 
    //        let v = 
    //            match sym with 
    //            | DeltaSim d -> d.nextDouble()
    //            | SymmetricTriangularSim d -> d.nextDouble()

    //        max v 0.0

    //    /// Returns values from 0 to 2 with max at 1.
    //    static member createDefault (seeder : unit -> int) = 
    //        SymmetricTriangularDistribution(seeder(), { threshold = None; scale = None; shift = Some 1.0 }) |> SymmetricTriangularSim

    //type SimDistributionGetter = 
    //    | DefaultSimDistributionGetter

    //    member sd.getDistr = 
    //        match sd with 
    //        | DefaultSimDistributionGetter -> SimDistribution.createDefault


    type RateMultiplierDistributionGetter =
        | DefaultRateMultiplierDistributionGetter

        member this.getDistr (d : RateMultiplierDistribution) (rate : double) = 
            match this with
            | DefaultRateMultiplierDistributionGetter -> d.scaled (Some rate) |> RateMultiplierDistribution
