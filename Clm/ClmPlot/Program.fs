open System
open Microsoft.FSharp.Core
open Clm.ModelInit
open Clm.ModelParams
open Clm.CommandLine
open Clm.SettingsExt
open Analytics.Visualization
open Argu
open Clm.Substances
open DbData.Configuration
open DbData.DatabaseTypes
open System.Data.SqlClient


[<EntryPoint>]
let main argv =
    printfn "%A" argv

    let resultDataId = 8L

    use conn = new SqlConnection(ClmConnectionString)

    match tryLoadResultData conn resultDataId with
    | Some r ->
        printfn "Plotting."
        let plotter = new Plotter(PlotDataInfo.defaultValue, r)
        plotter.plotAminoAcids()
        plotter.plotTotalSubst()
        plotter.plotEnantiomericExcess()
        printfn "Completed."
    | None ->
        printfn "Failed to load resultDataId: %A" resultDataId

    0
