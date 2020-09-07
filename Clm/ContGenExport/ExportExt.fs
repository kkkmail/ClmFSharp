namespace ContGenExport

open ClmSys.Export
open Clm.ModelParams
open Clm.ReactionRates

module ExportExt =
    
    type ReactionRateProviderParams
        with
        
        member p.export() =
            [ "", "" ]
    
    
    type ClmDefaultValue
        with
        
        member x.export() =
            0
            
            
    let x (d : list<ClmDefaultValue>) =
        d |> List.map (fun e -> e)
            