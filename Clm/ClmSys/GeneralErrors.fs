namespace ClmSys

/// Collection of general errors.
module GeneralErrors =

    type ReadFileError =
        | FileNotFound of string


    type JsonParseError =
        | InvalidStructure of string


    type ReadJsonError =
        | ReadFileError of ReadFileError
        | JsonParseError of JsonParseError


    type SerializationError =
        | X
