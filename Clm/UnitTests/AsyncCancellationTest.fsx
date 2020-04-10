open System.Threading
open System.Threading.Tasks
open System
open System.ComponentModel

let mutable counter = 0

/// Emulates a sync function that hung up.
let run() =
    while true
        do
            printfn "counter = %A" counter
            Thread.Sleep 1000
            counter <- counter + 1


let mutable cancel = false


let doAsyncTask (f : unit-> 'a) =
     async { return! Task<'a>.Factory.StartNew( new Func<'a>(f) ) |> Async.AwaitTask }


let sleeper (c : CancellationToken) = async {
    while not c.IsCancellationRequested
        do
            do! Async.Sleep 1000

    printfn "Exiting sleeper."
    return None
}


let sleeper3 () = async {
    while not cancel
        do
            do! Async.Sleep 1000

    printfn "Exiting sleeper3."
    return None
}



let onRunModel() =
    let c = new CancellationTokenSource()
    //let m = async { do run() }
    let m = doAsyncTask run
    Async.Start (m, c.Token)
    c


let onRunModel1() =
    let t = new Thread(fun () -> run())
    t.IsBackground <- true
    printfn "t.IsThreadPoolThread = %A" t.IsThreadPoolThread
    t


let onRunModel2() =
    let b = new BackgroundWorker()
    let doWork _ = run()
    b.DoWork.Add doWork
    b.WorkerSupportsCancellation <- true
    b


let onRunModel3() =
    let c = new CancellationTokenSource()

    let m = async {
        do run()
        return Some ()
    }

    let a = async {
                    let! x = Async.Choice [ sleeper3(); sleeper3() ]
                    do! Async.Sleep 0
                }

    Async.Start (a, c.Token)
    (c, a)


let tryCancel() =
    printfn "Starting..."
    let c = onRunModel()
    printfn "Waiting..."
    Thread.Sleep 5000
    printfn "Cancelling..."
    c.Cancel()
    printfn "Waiting again..."
    Thread.Sleep 5000
    printfn "Completed."


let tryCancel1() =
    printfn "Starting..."
    let t = onRunModel1()
    t.Start()
    printfn "Waiting..."
    Thread.Sleep 5000

    printfn "Cancelling..."

    try
        t.Abort()
    with
    | e -> printfn "Cancelled with exception: %A.\n Contunue further..." e

    printfn "Waiting again..."
    Thread.Sleep 5000
    printfn "Completed."


let tryCancel2() =
    printfn "Starting..."
    let t = onRunModel2()
    t.RunWorkerAsync()
    printfn "Waiting..."
    Thread.Sleep 5000

    printfn "Cancelling..."

    try
        t.CancelAsync()
    with
    | e -> printfn "Cancelled with exception: %A.\n Contunue further..." e

    printfn "Waiting again..."
    Thread.Sleep 5000
    printfn "Completed."


let tryCancel3() =
    printfn "Starting..."
    let (c, t) = onRunModel3()
    
    printfn "Waiting..."
    Thread.Sleep 5000
    printfn "Cancelling..."
    c.Cancel()
    cancel <- true
    printfn "Waiting again..."
    Thread.Sleep 5000
    printfn "Completed."


#time
tryCancel3()
#time
