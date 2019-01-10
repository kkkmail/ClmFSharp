namespace ProgressNotifierServiceInfo

module ServiceInfo =
    [<Literal>]
    let ProgressNotifierServiceName = "ProgressNotifierService"


    type TaskProgress =
        | NotStarted
        | InProgress of decimal
        | Completed

        static member create d =
            match d with 
            | _ when d <= 0.0m -> NotStarted
            | _ when d < 1.0m -> InProgress d
            | _ -> Completed


    type ProgressUpdateInfo =
        {
            updatedProcessId : int
            progress : TaskProgress
        }


    type IProgressNotifier =
        abstract notifyOfProgress : ProgressUpdateInfo -> unit

