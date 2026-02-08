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
        longRangeScanLines state, state

let phaserControl (state: GameState) =
    ["PHASER CONTROL -- NOT YET IMPLEMENTED"], state

let photonTorpedoControl (state: GameState) =
    ["PHOTON TORPEDO CONTROL -- NOT YET IMPLEMENTED"], state

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
    ["DAMAGE CONTROL REPORT -- NOT YET IMPLEMENTED"], state

let libraryComputer (state: GameState) =
    ["LIBRARY COMPUTER -- NOT YET IMPLEMENTED"], state

let help () : string list =
    let path = System.IO.Path.Combine("doc", "help.txt")
    if System.IO.File.Exists(path) then
        System.IO.File.ReadAllLines(path) |> Array.toList
    else
        ["HELP FILE NOT FOUND"]
