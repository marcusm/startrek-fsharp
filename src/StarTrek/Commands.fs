module StarTrek.App.Commands

open StarTrek.GameTypes
open StarTrek.Galaxy
open StarTrek.Enterprise

let warpEngineControl (state: GameState) =
    ["WARP ENGINE CONTROL -- NOT YET IMPLEMENTED"], state

let warpStart (state: GameState) : string list =
    if isWarpDamaged state.Enterprise then
        let maxWf = maxWarpFactor state.Enterprise
        [sprintf "WARP ENGINES ARE DAMAGED. MAXIMUM SPEED = WARP %g" maxWf]
    else
        []

let warpValidateCourse (input: string) : Result<float, string> =
    match System.Double.TryParse(input) with
    | true, course when course >= 1.0 && course < 9.0 -> Ok course
    | true, _ -> Error "  LT. SULU REPORTS, 'INCORRECT COURSE DATA, SIR!'"
    | false, _ -> Error "  LT. SULU REPORTS, 'INCORRECT COURSE DATA, SIR!'"

let warpValidateAndExecute (course: float) (input: string) (state: GameState) : string list * GameState =
    match System.Double.TryParse(input) with
    | true, wf when wf > 0.0 && wf <= maxWarpFactor state.Enterprise ->
        match getCourseVector course with
        | Some direction -> executeWarp direction wf state
        | None -> ["  LT. SULU REPORTS, 'INCORRECT COURSE DATA, SIR!'"], state
    | true, wf when wf > maxWarpFactor state.Enterprise ->
        [sprintf "CHIEF ENGINEER SCOTT REPORTS 'THE ENGINES WON'T TAKE WARP %g!'" wf], state
    | _ ->
        ["CHIEF ENGINEER SCOTT REPORTS 'THE ENGINES WON'T TAKE THAT, SIR!'"], state

let private isAdjacent (a: Position) (b: Position) =
    abs (a.X - b.X) <= 1 && abs (a.Y - b.Y) <= 1 && a <> b

let private isDocked (state: GameState) =
    let ep = state.Enterprise.Sector
    let sectorMap = state.CurrentQuadrant
    seq {
        for dy in -1..1 do
            for dx in -1..1 do
                if dx <> 0 || dy <> 0 then
                    let nx, ny = ep.X + dx, ep.Y + dy
                    if nx >= 1 && nx <= galaxySize && ny >= 1 && ny <= galaxySize then
                        yield sectorMap.[ny - 1, nx - 1]
    }
    |> Seq.exists (fun s -> match s with Starbase -> true | _ -> false)

let getCondition (state: GameState) =
    if state.Klingons.Length > 0 then "RED"
    elif isDocked state then "DOCKED"
    elif state.Enterprise.Energy + state.Enterprise.Shields < 1000.0 then "YELLOW"
    else "GREEN"

let shortRangeCommand (state: GameState) =
    let qx = state.Enterprise.Quadrant.X
    let qy = state.Enterprise.Quadrant.Y
    let condition = getCondition state
    [ sprintf "*** SHORT RANGE SENSOR SCAN FOR QUADRANT %d,%d ***" qx qy
      sprintf "    CONDITION: %s" condition ], state

let longRangeScan (state: GameState) =
    if isLongRangeScannersDamaged state.Enterprise then
        ["LONG RANGE SENSORS ARE INOPERABLE"], state
    else
        let lines, scanned = longRangeScan state
        lines, { state with QuadrantsScanned = scanned }

let phaserStart (state: GameState) : string list * bool =
    if isPhasersDamaged state.Enterprise then
        let repair = getPhaserRepairTime state.Enterprise
        [sprintf "PHASER DAMAGED, %d STARDATES ESTIMATED FOR REPAIR" repair], false
    elif state.Klingons.Length = 0 then
        ["PHASER FIRED AT EMPTY SPACE."], false
    else
        [sprintf "PHASERS LOCKED ON TARGET. ENERGY AVAILABLE = %.0f" state.Enterprise.Energy], true

let phaserValidateAndExecute (input: string) (state: GameState) : string list * GameState =
    match System.Double.TryParse(input) with
    | true, amount when amount > 0.0 ->
        if amount > state.Enterprise.Energy then
            [sprintf "SPOCK: WE HAVE ONLY %.0f ENERGY UNITS." state.Enterprise.Energy], state
        else
            firePhasers amount state
    | _ ->
        [], state

let torpedoStart (state: GameState) : string list * bool =
    if isPhotonTubesDamaged state.Enterprise then
        ["PHOTON TUBES ARE NOT OPERATIONAL"], false
    elif state.Enterprise.Torpedoes = 0 then
        ["ALL PHOTON TORPEDOES EXPENDED"], false
    else
        [sprintf "TORPEDO COURSE (1-9)? TORPEDOES LEFT: %d" state.Enterprise.Torpedoes], true

let torpedoValidateAndExecute (input: string) (state: GameState) : string list * GameState =
    match System.Double.TryParse(input) with
    | true, course when course >= 1.0 && course < 9.0 ->
        match getCourseVector course with
        | Some direction -> firePhotonTorpedo direction state
        | None -> ["ENSIGN CHEKOV REPORTS, 'INCORRECT COURSE DATA, SIR!'"], state
    | _ ->
        ["ENSIGN CHEKOV REPORTS, 'INCORRECT COURSE DATA, SIR!'"], state

let shieldControl (state: GameState) =
    if isShieldControlDamaged state.Enterprise then
        ["SHIELD CONTROL INOPERABLE"], state
    else
        let total = state.Enterprise.Energy + state.Enterprise.Shields
        [sprintf "ENERGY AVAILABLE = %g. NUMBER OF UNITS TO SHIELDS?" total], state

let shieldValidateAndExecute (input: string) (state: GameState) : string list * GameState =
    match System.Double.TryParse(input) with
    | true, requested ->
        match transferShields requested state.Enterprise with
        | Ok newEnterprise ->
            let newState = { state with Enterprise = newEnterprise }
            [sprintf "DEFLECTOR CONTROL ROOM REPORT:"; sprintf "  'SHIELDS NOW AT %g UNITS PER YOUR COMMAND.'" newEnterprise.Shields], newState
        | Error msg ->
            msg.Split('\n') |> Array.toList, state
    | false, _ ->
        ["INVALID. SHIELDS UNCHANGED"], state

let damageControlReport (state: GameState) =
    let damaged = getDamagedSystems state.Enterprise
    if damaged.IsEmpty then
        ["DAMAGE CONTROL REPORT: ALL SYSTEMS FUNCTIONAL"], state
    else
        let header = ["DAMAGE CONTROL REPORT:"]
        let lines =
            damaged
            |> List.map (fun (name, amount) -> sprintf "  %-22s %d" name amount)
        header @ lines, state

let libraryComputer (state: GameState) =
    if isComputerDamaged state.Enterprise then
        ["COMPUTER DISABLED"], state
    else
        ["COMPUTER ACTIVE AND AWAITING COMMAND"; "  0 = CUMULATIVE GALACTIC RECORD"], state

let libraryComputerOption (input: string) (state: GameState) =
    match input.Trim() with
    | "0" ->
        galacticRecordLines state, state
    | _ ->
        ["INVALID COMPUTER OPTION"], state

let help () : string list =
    let path = System.IO.Path.Combine("doc", "help.txt")
    if System.IO.File.Exists(path) then
        System.IO.File.ReadAllLines(path) |> Array.toList
    else
        ["HELP FILE NOT FOUND"]
