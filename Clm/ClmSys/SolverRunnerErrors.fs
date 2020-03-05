namespace ClmSys

open GeneralErrors
open ExitErrorCodes

module SolverRunnerErrors =

    type SolverRunnerCriticalError =
        {
            errorId : ErrorId
            commandLine : string[]
            result : int
            exceptionOpt : exn option
        }

        static member fromErrorCode c e =
            {
                errorId = ErrorId.getNewId()
                commandLine = c
                result = e
                exceptionOpt = None
            }

        static member fromExn c e =
            {
                errorId = ErrorId.getNewId()
                commandLine = c
                result = UnknownException
                exceptionOpt = Some e
            }
