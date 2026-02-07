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

let processCommand (input: string) (state: GameState) =
    match input.Trim().ToUpper() with
    | "0" -> Commands.warpEngineControl state
    | "1" -> Commands.shortRangeScan state
    | "2" -> Commands.longRangeScan state
    | "3" -> Commands.phaserControl state
    | "4" -> Commands.photonTorpedoControl state
    | "5" -> Commands.shieldControl state
    | "6" -> Commands.damageControlReport state
    | "7" -> Commands.libraryComputer state
    | "HELP" ->
        Commands.help ()
        state
    | _ ->
        printfn "INVALID COMMAND. ENTER 0-7 OR HELP."
        state

let rec gameLoop (state: GameState) =
    printfn ""
    Display.printShortRangeScan state
    printf "COMMAND? "
    let input = System.Console.ReadLine()
    if input = null || input.Trim().ToUpper() = "Q" then
        printfn "GOODBYE."
    else
        let state = processCommand input state
        gameLoop state

[<EntryPoint>]
let main argv =
    let state = createInitialState ()
    gameLoop state
    0
