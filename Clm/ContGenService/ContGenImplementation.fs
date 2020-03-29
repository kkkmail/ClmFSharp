namespace ContGenService

open Argu
open ContGenServiceInfo.ServiceInfo
open SvcCommandLine

module ServiceImplementation =

    let parserResults =
        let parser = ArgumentParser.Create<ContGenRunArgs>(programName = contGenServiceProgramName)
        (parser.Parse [||]).GetAllResults()


    let mutable serviceAccessInfo = getServiceAccessInfo parserResults
