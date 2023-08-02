namespace ClmSys

open ClmSys.GeneralData

module Export =

    /// Takes functions, which produce column header and column data,
    /// and converts a list of lists into a list of unique column names and
    /// a map of data for each row.
    let toRows h r a =
        let b = a |> List.map (fun e -> e |> List.map (fun x -> (h x, r x)))
        let header = b |> List.map (fun e -> e |> List.map fst) |> List.concat |> List.distinct
        let rows = b |> List.map (fun e -> e |> Map.ofList)
        header, rows
    
    
    /// Converts row of elements into export string.
    let exportRow f a =
        (a |> List.fold (fun acc r -> (if acc = EmptyString then acc else acc + ",") + f r) EmptyString) + Nl
        
        
    /// Converts row of optional elements into export string.
    let exportOptRow (f, g) a =
        exportRow (fun e -> match e with | Some v -> f v | None -> g()) a
            
            
    /// Export prepared data into a list of strings.
    let toExportData f g data =
        let (h, r) = data
        let header = h |> exportRow f
        let rows = r |> List.map (fun e -> h |> List.map (fun a -> e |> Map.tryFind a) |> exportOptRow g)
        header :: rows 
        