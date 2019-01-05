namespace Clm
open System

module GeneralData =

    /// String.Empty is not a const.
    [<Literal>]
    let EmptyString = ""


    /// Environment.NewLine is too long and it is not a const.
    [<Literal>]
    let Nl = "\r\n"


    let toVariableName (s : string) =
        match s.Length with
        | 0 -> s
        | 1 -> s.ToLower()
        | _ -> s.Substring(0, 1).ToLower() + s.Substring(1)


    let getRandomSeeder (rnd : Random) (seed : int option) = rnd.Next ()


    let getDeterministicSeeder (rnd : Random) (seed : int option) =
        match seed with
        | Some s -> s
        | None -> rnd.Next ()
