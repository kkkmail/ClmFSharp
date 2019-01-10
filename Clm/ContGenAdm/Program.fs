open ContGenAdm.ContGenServiceResponse
open System.Threading

[<EntryPoint>]
let main argv =
    let service = new ContGenResponseHandler()

    try
        service.contGenService.startGenerating()

        while true do
            let state = service.contGenService.getState()
            printfn "state = %A" state
            Thread.Sleep(10_000)
    with
        | e -> printfn "Exception: %A" e.Message
    0
