namespace ClmSys

open GeneralErrors

module ContGenErrors =

    type TryCancelRunQueueError =
        | TryCancelRunQueueWcfErr of WcfError


    type TryRequestResultsError =
        | TryRequestResultsWcfErr of WcfError


    type ContGenServiceError =
        | TryCancelRunQueueErr of TryCancelRunQueueError
        | TryRequestResultsErr of TryRequestResultsError
