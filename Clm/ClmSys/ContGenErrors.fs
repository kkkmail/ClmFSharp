namespace ClmSys

open GeneralErrors

module ContGenErrors =

    type TryDeleteRunQueueError =
        | TryDeleteRunQueueWcfErr of WcfError


    type ContGenServiceError =
        | TryDeleteRunQueueErr of TryDeleteRunQueueError
