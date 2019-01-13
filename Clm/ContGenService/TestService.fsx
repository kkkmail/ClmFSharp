#r @"bin\debug\DiscoverData.dll"
#r @"bin\debug\DiscoverEngine.dll"
#r @"bin\debug\DiscoverService.exe"

open System
open DiscoverData.DataTypes
//open DiscoverData.AllData
//open DiscoverData.DynamicData
//open DiscoverData.DataUtils
//open DiscoverEngine.DiscoverTypes
//open DiscoverEngine.DiscoverEngineTypes
//open DiscoverEngine.DiscoverEngineConfig
//open DiscoverEngine.DiscoverCollections
//open DiscoverEngine.DRE
//open DiscoverEngine.ProfileAdjuster
//open DiscoverData.DataSources

open DiscoverService.ServiceInterface


// Get remote instance of the service object
let url = "tcp://localhost:" + (DiscoverServicePort.ToString ()) + "/" + DiscoverServiceName
//let url = "http://localhost:" + (DiscoverServicePort.ToString ()) + "/" + DiscoverServiceName
//let url = "ipc://abc/" + DiscoverServiceName

printfn "url = %s" url

let service : IDiscoverService = 
    Activator.GetObject (typeof<IDiscoverService>, url) 
    :?> IDiscoverService



printfn "Test the service - send message"
service.SendMessage("Welcome to F# chat!")
service.SendMessage("Another chat message...")

printfn "Test the service - receive message"
let msg = service.GetMessages()
printfn "msg = %s" msg

//let profile : userProfile = 
//    {userProfile.Default with
//        userProfile.scaleOfGPA = Some ScaleOfGPA.Scale0to4 // Must say that!!!
//        userProfile.gpaEngOnScale0to4 = Some 2.3;
//        userProfile.gpaMathOnScale0to4 = Some 2.3;
//        gender = Some Gender.Male;
//    }


let userAnswers = 
    [
        ("takenStandardisedTest", "sat");
        ("takenStandardisedTest###sat###satMath", "800");
        ("takenStandardisedTest###sat###satEng", "800");
        ("importanceOfCostLow", "veryImportant");
        ("amtCanPay", "7500");
        ("familyIncome", "30000");
        ("residencyState", "CA");

//        ("likelyAcademicStanding", "amongTheBest")
//        ("likelyAcademicStanding", "betterThanMany")
        ("likelyAcademicStanding", "average")
    ]

printfn "RespondWithCompleteMatches..."
//let matches = Option.ofObj (service.RespondWithCompleteMatches (ConfigurationParams.Default, profile))
//let m = service.RespondWithCompleteMatches (ConfigurationParams.Default, profile)
//service.RespondWithCompleteMatches (ConfigurationParams.Default, profile) |> ignore
//service.RespondWithCompleteMatches (ConfigurationParams.Default, profile) |> ignore

let getMatches (qa : #seq<string * string>) : list<Institution> = 
    let response = service.GetResponseHandle qa

    let rec getNextMatch (matches : list<Institution>) : list<Institution> = 
        match service.GetNextResponse response.handle with
        | Some m -> getNextMatch (m :: matches)
        | None -> matches

    getNextMatch []


printfn "... completed"
//printfn "response = %A" response

let matches = getMatches userAnswers

//match matches with 
//| Some m -> printfn "matches.Length = %i\n\n" (m.Length)
//| None -> printfn "!!!  GOT NULL !!!"

printfn "matches = "
matches |> List.map (fun m -> printfn "    %A" m)

printfn "\n\nmatches.Length = %A" (matches.Length)

