namespace ClmSys

open GeneralErrors
open ExitErrorCodes

module SolverRunnerErrors =
    let x = 1

    //type CriticalErrorType =
    //    | ErrorCodeBased
    //    | ExceptionBased
    //    | ErrroMessageBased


    //type SolverRunnerCriticalError =
    //    {
    //        errorId : ErrorId
    //        errorType : CriticalErrorType
    //        commandLine : string[]
    //        result : int
    //        errorMessageOpt : string option
    //        exceptionOpt : exn option
    //    }

    //    static member fromErrorCode c e =
    //        {
    //            errorId = ErrorId.getNewId()
    //            errorType = ErrorCodeBased
    //            commandLine = c
    //            result = e
    //            errorMessageOpt = None
    //            exceptionOpt = None
    //        }

    //    static member fromExn c e =
    //        {
    //            errorId = ErrorId.getNewId()
    //            errorType = ExceptionBased
    //            commandLine = c
    //            result = UnknownException
    //            errorMessageOpt = None
    //            exceptionOpt = Some e
    //        }


    //    static member fromErrMessage c e =
    //        {
    //            errorId = ErrorId.getNewId()
    //            errorType = ErrroMessageBased
    //            commandLine = c
    //            result = UnknownException
    //            errorMessageOpt = Some e
    //            exceptionOpt = None
    //        }
