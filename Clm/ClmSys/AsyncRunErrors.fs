namespace ClmSys

module AsyncRunErrors =

    type OnStartRunError =
        | FailedToStartErr //of RunningProcessData

    type AsyncRunError =
        | OnStartRunErr of OnStartRunError

