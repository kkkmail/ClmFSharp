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
        | SerializationException of exn
        | DeserializationException of exn


    type WcfError =
        | WcfException of exn
        | WcfSerializationError of SerializationError
