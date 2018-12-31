open Clm.Distributions
open Clm.Substances
open Clm.ReactionRates

open ContGen.Configuration
open ContGen.DatabaseTypes
open ContGen.Settings
open ContGen.SettingsExt
open System


[<EntryPoint>]
let main argv = 
    let rnd = new Random()

    let rates = ReactionRateProvider.getDefaultRates rnd TwoAminoAcids

    let x = rates |> List.head
    let y = x.inputParams

    printfn "%A" argv
    0 // return an integer exit code
