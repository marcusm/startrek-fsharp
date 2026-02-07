open StarTrek
open StarTrek.Galaxy
open StarTrek.App

let createInitialSectorMap () =
    let map = createEmptySectorMap ()
    map.[0, 1] <- Star
    map.[2, 5] <- Star
    map.[2, 7] <- Star
    map.[3, 4] <- Enterprise
    map.[4, 1] <- Star
    map.[7, 6] <- Star
    map

let createInitialState () =
    { Quadrant = { X = 5; Y = 2 }
      Sector = { X = 5; Y = 4 }
      Energy = 2500.0
      Shields = 500.0
      Torpedoes = 10
      Stardate = 2100.0
      TimeRemaining = 30.0
      KlingonsRemaining = 0
      BasesRemaining = 0
      Devices = GameDefaults.initialDeviceStatus
      QuadrantMap = Array2D.create galaxySize galaxySize { Klingons = 0; Starbases = 0; Stars = 0 }
      SectorMap = createInitialSectorMap ()
      Condition = "GREEN" }

[<EntryPoint>]
let main argv =
    let state = createInitialState ()
    TerminalUI.run state
    0
