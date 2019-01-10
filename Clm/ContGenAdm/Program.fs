open ContGenAdm.ContGenServiceResponse
open System.Threading

[<EntryPoint>]
let main argv =
    let service = new ContGenResponseHandler()

    //service.contGenService.startGenerating()

    while true do
        try
            printfn "Getting state..."
            let state = service.contGenService.getState()
            printfn "...state = %A\n\n" state
        with
            | e -> printfn "Exception: %A\n" e.Message

        Thread.Sleep(5_000)

    0
