namespace Clm
open System

module Distributions = 

    type DistributionParams = 
        {
            threshold : double option
        }

        static member defaultValue = 
            {
                threshold = None
            }


    [<AbstractClass>]
    type DistributionBase(seed : int, p : DistributionParams, d : Random -> double) = 
        let rnd = new Random(seed)
        let rndBool = new Random(rnd.Next())

        let isDefinedImpl() = 
            match p.threshold with
            | Some t -> if rndBool.NextDouble() < t then true else false
            | None -> true

        let nextDoubleImpl() = d(rnd)
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


    type Distribution =
        | Delta of DeltaDistribution
        | Uniform of UniformDistribution
        | Triangular of TriangularDistribution

        member this.nextDouble = 
            match this with
            | Delta d -> d.nextDouble
            | Uniform d -> d.nextDouble
            | Triangular d -> d.nextDouble

        member this.nextDoubleOpt = 
            match this with
            | Delta d -> d.nextDoubleOpt
            | Uniform d -> d.nextDoubleOpt
            | Triangular d -> d.nextDoubleOpt

        member this.nextDoubleFromZeroToOne =
            match this with
            | Delta d -> d.nextDoubleFromZeroToOne
            | Uniform d -> d.nextDoubleFromZeroToOne
            | Triangular d -> d.nextDoubleFromZeroToOne

        member this.isDefined =
            match this with
            | Delta d -> d.isDefined
            | Uniform d -> d.isDefined
            | Triangular d -> d.isDefined


    /// Specially formatted distributions to return values only between (-1 and 1).
    type EeDistribution = 
        | EeDistribution

        member this.nextDouble() : double = failwith "EeDistribution.nextDouble is not yet implemented."
