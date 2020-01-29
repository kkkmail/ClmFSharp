namespace ClmSys

open GeneralData
open ContGenPrimitives

module ContGenData =

    type ContGenServiceAccessInfo =
        {
            contGenServiceAccessInfo : ServiceAccessInfo
            minUsefulEe : MinUsefulEe
        }
