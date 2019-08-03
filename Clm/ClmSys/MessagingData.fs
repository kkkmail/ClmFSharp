namespace ClmSys

open System
open ClmSys.GeneralData

module MessagingData =

    type MessagingClientName =
        | Unnamed
        | Named of string

        //member this.name =
        //    match this with
        //    | Unnamed -> EmptyString
        //    | Named n -> n

        static member create s =
            if s = EmptyString then Unnamed
            else Named s


    type MessagingClientId =
        | MessagingClientId of Guid

        member this.value = let (MessagingClientId v) = this in v
        static member create() = Guid.NewGuid() |> MessagingClientId
