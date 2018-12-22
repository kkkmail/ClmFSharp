namespace Clm
open System

module Distributions = 

    type DistributionParams = 
        {
            threshold : double option
            shift : double option
        }

        static member defaultValue = 
            {
                threshold = None
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
            d(rnd) + 
            match p.shift with 
            | Some s -> s
            | None -> 0.0

        let nextDoubleFromZeroToOneImpl() = rnd.NextDouble()

        member __.seedValue = seed
        member __.distributionParams = p
        member __.nextDouble = nextDoubleImpl
        member __.nextDoubleFromZeroToOne = nextDoubleFromZeroToOneImpl

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


    /// Specially formatted distributions to return values only between (-1 and 1).
    type EeDistribution = 
        | SymmetricTriangularEe of SymmetricTriangularDistribution

        member eed.nextDouble() : double = 
            match eed with 
            | SymmetricTriangularEe d -> d.nextDouble()

        static member createDefault (rnd : Random) = 
            SymmetricTriangularDistribution(rnd.Next(), { threshold = None; shift = None }) |> SymmetricTriangularEe

        static member createDefaultOpt (rnd : Random) = EeDistribution.createDefault rnd |> Some


    /// Specially formatted distributions to return values above 0 and with max / mean at 1.
    type SimDistribution = 
        | DeltaSim of DeltaDistribution
        | SymmetricTriangularSim of SymmetricTriangularDistribution

        member sym.nextDouble() : double = 
            match sym with 
            | DeltaSim d -> d.nextDouble()
            | SymmetricTriangularSim d -> d.nextDouble()

        /// Returns values from 0 to 2 with max at 1.
        static member createDefault (rnd : Random) = 
            SymmetricTriangularDistribution(rnd.Next(), { threshold = None; shift = Some 1.0 }) |> SymmetricTriangularSim

        static member createDefaultOpt (rnd : Random) = SimDistribution.createDefault rnd |> Some
