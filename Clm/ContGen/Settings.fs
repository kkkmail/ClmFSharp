namespace ContGen
open System.Data.SqlClient
open FSharp.Data

open Configuration

module Settings = 

    type ClmDB = SqlProgrammabilityProvider<ClmSqlProviderName, ConfigFile = AppConfigFile>

    type Setting = 
        | X of int
