open System

let a = DateTime.Now
let b = a.AddDays(1.21111)
let diff = b - a

let formatTimeSpan (t : TimeSpan) =
    let x = sprintf "%i:%02i:%02i" diff.Hours diff.Minutes diff.Seconds

    if t.Days = 0
    then x
    else sprintf "%i day(s), %s" diff.Days x

diff |> formatTimeSpan |> printfn "diff = %s"
