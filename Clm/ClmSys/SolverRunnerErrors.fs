namespace ClmSys

open GeneralErrors
open ExitErrorCodes

module SolverRunnerErrors =

    type SolverRunnerCriticalError =
        {
            errorId : ErrorId
            commandLine : string[]
            result : int
            errorMessageOpt : string option
            exceptionOpt : exn option
        }

        static member fromErrorCode c e =
            {
                errorId = ErrorId.getNewId()
                commandLine = c
                result = e
                errorMessageOpt = None
                exceptionOpt = None
            }

        static member fromExn c e =
            {
                errorId = ErrorId.getNewId()
                commandLine = c
                result = UnknownException
                errorMessageOpt = None
                exceptionOpt = Some e
            }


        static member fromErrMessage c e =
            {
                errorId = ErrorId.getNewId()
                commandLine = c
                result = UnknownException
                errorMessageOpt = Some e
                exceptionOpt = None
            }
