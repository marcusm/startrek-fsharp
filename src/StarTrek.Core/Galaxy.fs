module StarTrek.Galaxy

open StarTrek.GameTypes
open StarTrek.Enterprise

let galaxySize = 8

let defaultKlingonEnergy = 200.0

let createEmptySectorMap () : Sector[,] =
    Array2D.create galaxySize galaxySize Empty

let encodeQuadrant (quadrant: Quadrant) : int =
    quadrant.Klingons * 100 + quadrant.Starbases * 10 + quadrant.Stars

let decodeQuadrant (code: int) : Quadrant =
    { Quadrant = { X = 0; Y = 0 }
      Klingons = code / 100
      Starbases = (code % 100) / 10
      Stars = code % 10 }

let isValidPosition (pos: Position) =
    pos.X >= 1 && pos.X <= galaxySize && pos.Y >= 1 && pos.Y <= galaxySize

let sectorToChar (sector: Sector) =
    match sector with
    | Empty -> "   "
    | Star -> " * "
    | Klingon _ -> "+++"
    | Starbase -> ">!<"
    | Enterprise -> "<*>"

let courseDirections =
    [| (0.0, 1.0)     // Course 1: East (right)
       (-1.0, 1.0)    // Course 2: NE
       (-1.0, 0.0)    // Course 3: North (up)
       (-1.0, -1.0)   // Course 4: NW
       (0.0, -1.0)    // Course 5: West (left)
       (1.0, -1.0)    // Course 6: SW
       (1.0, 0.0)     // Course 7: South (down)
       (1.0, 1.0)     // Course 8: SE
    |]

let getCourseVector (course: float) =
    if course < 1.0 || course >= 9.0 then
        None
    else
        let idx = int (course - 1.0)
        let frac = course - float (int course)
        let (dy1, dx1) = courseDirections.[idx % 8]
        let (dy2, dx2) = courseDirections.[(idx + 1) % 8]
        let dx = dx1 + frac * (dx2 - dx1)
        let dy = dy1 + frac * (dy2 - dy1)
        Some (dx, dy)

let private formatRow (cells: string list) =
    cells |> String.concat " : " |> sprintf ": %s :"

let isLongRangeScannersDamaged (enterprise: Enterprise) : bool =
    enterprise.Damage
    |> List.exists (fun d -> d.System = LongRangeSensors && d.Amount < 0)

let longRangeScan (state: GameState) : string list * Set<Position> =
    let qx = state.Enterprise.Quadrant.X
    let qy = state.Enterprise.Quadrant.Y
    let separator = "-------------------"
    let header = sprintf "LONG RANGE SCAN FOR QUADRANT %d,%d" qx qy
    let neighbors =
        [ for dy in -1 .. 1 do
            for dx in -1 .. 1 do
                { X = qx + dx; Y = qy + dy } ]

    let scanned =
        neighbors
        |> List.filter isValidPosition
        |> Set.ofList
        |> Set.union state.QuadrantsScanned

    let lines =
        [
            header
            separator
            for dy in -1 .. 1 do
                let cells =
                    [ for dx in -1 .. 1 do
                        let pos = { X = qx + dx; Y = qy + dy }
                        if isValidPosition pos then
                            let q = state.Quadrants.[pos.X - 1, pos.Y - 1]
                            sprintf "%03d" (encodeQuadrant q)
                        else
                            "***" ]
                formatRow cells
                separator
        ]
    lines, scanned

let warpEnergyCost (warpFactor: float) : int =
    if warpFactor < 1.0 then 1
    else int (floor (warpFactor * 8.0)) - 5

let isWarpDamaged (enterprise: Enterprise) : bool =
    enterprise.Damage
    |> List.exists (fun d -> d.System = WarpEngines && d.Amount < 0)

let maxWarpFactor (enterprise: Enterprise) : float =
    if isWarpDamaged enterprise then 0.2 else 8.0

let rec private findEmptyPosition (random: IRandomService) (occupied: Set<Position>) =
    let pos = { X = random.Next(1, 9); Y = random.Next(1, 9) }
    if Set.contains pos occupied then findEmptyPosition random occupied
    else pos

let private placeRandomItems (random: IRandomService) (count: int) (occupied: Set<Position>) =
    (([], occupied), [1..count])
    ||> List.fold (fun (placed, occ) _ ->
        let pos = findEmptyPosition random occ
        (pos :: placed, Set.add pos occ))

let galacticRecordLines (state: GameState) : string list =
    let separator = "-------------------------------------------------"
    [
        "        CUMULATIVE GALACTIC RECORD"
        separator
        for y in 0 .. galaxySize - 1 do
            let cells =
                [ for x in 0 .. galaxySize - 1 do
                    let pos = { X = x + 1; Y = y + 1 }
                    if Set.contains pos state.QuadrantsScanned then
                        let q = state.Quadrants.[x, y]
                        sprintf "%03d" (encodeQuadrant q)
                    else
                        "???" ]
            formatRow cells
            separator
    ]

let statusReportLines (state: GameState) : string list =
    let totalKlingons =
        state.Quadrants
        |> Seq.cast<Quadrant>
        |> Seq.sumBy (fun q -> q.Klingons)
    let totalStarbases =
        state.Quadrants
        |> Seq.cast<Quadrant>
        |> Seq.sumBy (fun q -> q.Starbases)
    let stardatesLeft =
        state.Stardate.Turns - (state.Stardate.Current - state.Stardate.Start)
    [
        "   STATUS REPORT"
        sprintf "KLINGON SHIPS LEFT : %d" totalKlingons
        sprintf "STARDATES LEFT     : %d" stardatesLeft
        sprintf "STARBASES LEFT     : %d" totalStarbases
    ]

let totalKlingonsRemaining (state: GameState) : int =
    state.Quadrants
    |> Seq.cast<Quadrant>
    |> Seq.sumBy (fun q -> q.Klingons)

let stardatesRemaining (state: GameState) : int =
    state.Stardate.Turns - (state.Stardate.Current - state.Stardate.Start)

let efficiencyRating (state: GameState) : float =
    let timeUsed = float (state.Stardate.Current - state.Stardate.Start)
    if timeUsed <= 0.0 then
        float state.InitialKlingons * 1000.0
    else
        float state.InitialKlingons / timeUsed * 1000.0

let checkGameEnd (state: GameState) : GameEndCondition option =
    let klingonsLeft = totalKlingonsRemaining state
    if klingonsLeft = 0 then
        Some Victory
    elif stardatesRemaining state <= 0 then
        Some TimeExpired
    elif condition state.Enterprise = Destroyed then
        Some PlayerDestroyed
    elif condition state.Enterprise = DeadInSpace && state.Klingons.Length > 0 then
        Some PlayerDeadInSpace
    else
        None

let victoryMessages (state: GameState) : string list =
    let rating = efficiencyRating state
    [
        "THE LAST KLINGON BATTLE CRUISER IN THE GALAXY HAS BEEN DESTROYED."
        "THE FEDERATION HAS BEEN SAVED."
        ""
        "YOU HAVE BEEN PROMOTED TO ADMIRAL."
        sprintf "%.2f IS YOUR EFFICIENCY RATING." rating
    ]

let timeExpiredMessages (state: GameState) : string list =
    let klingonsLeft = totalKlingonsRemaining state
    [
        sprintf "IT IS STARDATE %d." state.Stardate.Current
        sprintf "THERE ARE STILL %d KLINGON BATTLE CRUISERS." klingonsLeft
        "YOU HAVE BEEN RELIEVED OF COMMAND."
    ]

let destroyedMessages () : string list =
    [
        "THE ENTERPRISE HAS BEEN DESTROYED. THE FEDERATION WILL BE CONQUERED."
    ]

let deadInSpaceMessages () : string list =
    [
        "THE ENTERPRISE IS DEAD IN SPACE. KLINGONS CLOSE IN."
        "YOU HAVE BEEN RELIEVED OF COMMAND --"
        "DEMOTED TO THE RANK OF LIEUTENANT."
    ]

let torpedoDataLines (state: GameState) : string list =
    if state.Klingons.Length = 0 then
        ["SCIENCE OFFICER SPOCK REPORTS:"; "  'SENSORS SHOW NO ENEMY SHIPS IN THIS QUADRANT.'"]
    else
        let ep = state.Enterprise.Sector
        [
            "PHOTON TORPEDO DATA:"
            for klingon in state.Klingons do
                let dx = float (klingon.Sector.X - ep.X)
                let dy = float (klingon.Sector.Y - ep.Y)
                let distance = System.Math.Sqrt(dx * dx + dy * dy)
                // Course system: 1=E, 2=NE, 3=N, 4=NW, 5=W, 6=SW, 7=S, 8=SE
                // atan2(dy, dx) gives angle from +X axis (East), counterclockwise positive
                // Courses go counterclockwise in screen coords (Y-down), so negate the angle
                let angle = System.Math.Atan2(dy, dx)
                let course = 1.0 - angle * 4.0 / System.Math.PI
                // Normalize to [1, 9)
                let course = if course < 1.0 then course + 8.0 else course
                sprintf "  KLINGON AT SECTOR %d,%d: COURSE = %.1f  DISTANCE = %.1f"
                    klingon.Sector.X klingon.Sector.Y course distance
        ]

let isDocked (state: GameState) : bool =
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

let checkDocking (state: GameState) : string list * GameState =
    if isDocked state then
        let enterprise = state.Enterprise
        let resupplied =
            { enterprise with
                Energy = 3000.0
                Torpedoes = 10
                Shields = 0.0 }
        ["SHIELDS DROPPED FOR DOCKING PURPOSES"], { state with Enterprise = resupplied }
    else
        [], state

/// §4.3 Red Alert on Entry: warn when entering a quadrant with Klingons and low shields
let redAlertMessages (state: GameState) : string list =
    if state.Klingons.Length > 0 && state.Enterprise.Shields <= 200.0 then
        ["COMBAT AREA      CONDITION RED"
         "   SHIELDS DANGEROUSLY LOW"]
    else
        []

let private placeQuadrantEntities (random: IRandomService) (enterprisePos: Position) (quadrant: Quadrant) =
    let occupied = Set.singleton enterprisePos
    let klingonPositions, occupied = placeRandomItems random quadrant.Klingons occupied
    let starbasePositions, occupied = placeRandomItems random quadrant.Starbases occupied
    let starPositions, _ = placeRandomItems random quadrant.Stars occupied
    klingonPositions, starbasePositions, starPositions

let private buildKlingonShips (positions: Position list) : Klingon array =
    positions
    |> List.map (fun pos -> { Sector = pos; Energy = defaultKlingonEnergy })
    |> List.toArray

let private buildSectorMap (enterprisePos: Position) (klingonPos: Position list) (starbasePos: Position list) (starPos: Position list) : Sector[,] =
    let positionMap =
        [ yield (enterprisePos, Enterprise)
          for pos in klingonPos do yield (pos, Klingon defaultKlingonEnergy)
          for pos in starbasePos do yield (pos, Starbase)
          for pos in starPos do yield (pos, Star) ]
        |> Map.ofList
    Array2D.init galaxySize galaxySize (fun row col ->
        let pos = { X = col + 1; Y = row + 1 }
        match Map.tryFind pos positionMap with
        | Some sector -> sector
        | None -> Empty)

let enterQuadrant (state: GameState) : string list * GameState =
    let qx = state.Enterprise.Quadrant.X - 1
    let qy = state.Enterprise.Quadrant.Y - 1
    let quadrant = state.Quadrants.[qx, qy]
    let enterprisePos = state.Enterprise.Sector

    let klingonPos, starbasePos, starPos = placeQuadrantEntities state.Random enterprisePos quadrant
    let klingons = buildKlingonShips klingonPos
    let sectorMap = buildSectorMap enterprisePos klingonPos starbasePos starPos
    let scanned = Set.add state.Enterprise.Quadrant state.QuadrantsScanned

    let newState = { state with CurrentQuadrant = sectorMap; Klingons = klingons; QuadrantsScanned = scanned }
    let dockMsgs, dockedState = checkDocking newState
    let alertMsgs = redAlertMessages dockedState
    dockMsgs @ alertMsgs, dockedState

type private WalkResult =
    | Arrived of Position
    | ExitedQuadrant of Position
    | Blocked of Position

let private walkSectors (sectorMap: Sector[,]) (direction: float * float) (numSteps: int) (startX: float) (startY: float) (startPos: Position) : WalkResult =
    let dx, dy = direction
    let rec step n (fx: float) (fy: float) (prev: Position) =
        if n > numSteps then
            Arrived prev
        else
            let nx = fx + dx
            let ny = fy + dy
            let sx = int (System.Math.Round(nx))
            let sy = int (System.Math.Round(ny))
            if sx < 1 || sx > galaxySize || sy < 1 || sy > galaxySize then
                ExitedQuadrant prev
            else
                match sectorMap.[sy - 1, sx - 1] with
                | Empty -> step (n + 1) nx ny { X = sx; Y = sy }
                | _ -> Blocked prev
    step 1 startX startY startPos

let private resolveExitPosition (quadrant: Position) (startX: float) (startY: float) (direction: float * float) (numSteps: int) =
    let dx, dy = direction
    let absX = float (quadrant.X - 1) * 8.0 + startX + dx * float numSteps
    let absY = float (quadrant.Y - 1) * 8.0 + startY + dy * float numSteps
    let newQX = int (System.Math.Floor(absX / 8.0)) + 1
    let newQY = int (System.Math.Floor(absY / 8.0)) + 1
    let clampedQX = max 1 (min galaxySize newQX)
    let clampedQY = max 1 (min galaxySize newQY)
    let hitEdge = clampedQX <> newQX || clampedQY <> newQY
    { X = clampedQX; Y = clampedQY }, hitEdge

let private applyRepairCycle (state: GameState) : string list * GameState =
    let repaired = automaticRepair state.Enterprise
    let msgs, repaired = randomDamageEvent state.Random repaired
    msgs, { state with Enterprise = repaired }

let private andThen (f: GameState -> string list * GameState) (msgs: string list, state: GameState) : string list * GameState =
    let moreMsgs, newState = f state
    msgs @ moreMsgs, newState

let private perimeterDenialMessages (lastPos: Position) (newQuadrant: Position) =
    ["LT. UHURA REPORTS MESSAGE FROM STARFLEET COMMAND:";
     "  'PERMISSION TO ATTEMPT CROSSING OF GALACTIC PERIMETER";
     "  IS HEREBY *DENIED*. SHUT DOWN YOUR ENGINES.'";
     "CHIEF ENGINEER SCOTT REPORTS 'WARP ENGINES SHUT DOWN";
     sprintf "  AT SECTOR %d,%d OF QUADRANT %d,%d.'" lastPos.X lastPos.Y newQuadrant.X newQuadrant.Y]

let executeWarp (direction: float * float) (warpFactor: float) (state: GameState) : string list * GameState =
    let numSteps = int (warpFactor * 8.0 + 0.5)
    let energyCost = float (warpEnergyCost warpFactor)

    if state.Enterprise.Energy - state.Enterprise.Shields < energyCost then
        ["ENGINEERING REPORTS: 'INSUFFICIENT ENERGY AVAILABLE"; sprintf "                      FOR MANEUVERING AT WARP %g'!" warpFactor], state
    else
        let sectorMap = Array2D.copy state.CurrentQuadrant
        let ep = state.Enterprise.Sector
        sectorMap.[ep.Y - 1, ep.X - 1] <- Empty
        let startX = float ep.X - 0.5
        let startY = float ep.Y - 0.5

        let deductAndAdvance newEnterprise =
            { state with
                Enterprise = { newEnterprise with Energy = state.Enterprise.Energy - energyCost }
                Stardate = { state.Stardate with Current = state.Stardate.Current + 1 } }

        match walkSectors sectorMap direction numSteps startX startY ep with
        | ExitedQuadrant lastPos ->
            let newQuadrant, hitEdge = resolveExitPosition state.Enterprise.Quadrant startX startY direction numSteps
            let edgeMsgs = if hitEdge then perimeterDenialMessages lastPos newQuadrant else []
            let newSectorPos = findEmptyPosition state.Random Set.empty
            let newEnterprise = { state.Enterprise with Quadrant = newQuadrant; Sector = newSectorPos }
            (edgeMsgs, deductAndAdvance newEnterprise)
            |> andThen applyRepairCycle
            |> andThen enterQuadrant

        | Blocked pos ->
            sectorMap.[pos.Y - 1, pos.X - 1] <- Enterprise
            let newEnterprise = { state.Enterprise with Sector = pos }
            let msgs = ["WARP ENGINES SHUT DOWN AT SECTOR"; sprintf "%d,%d DUE TO BAD NAVIGATION" pos.X pos.Y]
            (msgs, { deductAndAdvance newEnterprise with CurrentQuadrant = sectorMap })
            |> andThen applyRepairCycle
            |> andThen checkDocking

        | Arrived pos ->
            sectorMap.[pos.Y - 1, pos.X - 1] <- Enterprise
            let newEnterprise = { state.Enterprise with Sector = pos }
            ([], { deductAndAdvance newEnterprise with CurrentQuadrant = sectorMap })
            |> andThen applyRepairCycle
            |> andThen checkDocking

let private generateKlingonCount (random: IRandomService) =
    let chance = random.NextDouble()
    match chance with
    | c when c > 0.98 -> 3
    | c when c > 0.95 -> 2
    | c when c > 0.80 -> 1
    | _ -> 0

let private generateStarbaseCount (random: IRandomService) =
    if random.NextDouble() < 0.04 then 1 else 0

let private generateStarCount (random: IRandomService) =
    int (floor (random.NextDouble() * 8.0 + 1.0))

let private placeEnterprise (random: IRandomService) =
    let quadrant = { X = random.Next(1, 9); Y = random.Next(1, 9) }
    let sector = { X = random.Next(1, 9); Y = random.Next(1, 9) }
    resetEnterprise quadrant sector

let rec private tryGenerateGame (random: IRandomService) : GameState =
    let quadrants =
        Array2D.init galaxySize galaxySize (fun x y ->
            { Quadrant = { X = x + 1; Y = y + 1 }
              Klingons = generateKlingonCount random
              Starbases = generateStarbaseCount random
              Stars = generateStarCount random })

    let totalKlingons, totalStarbases =
        quadrants
        |> Seq.cast<Quadrant>
        |> Seq.fold (fun (k, b) q -> (k + q.Klingons, b + q.Starbases)) (0, 0)

    if totalKlingons < 12 || totalStarbases < 2 then
        tryGenerateGame random
    else
        let enterprise = placeEnterprise random

        let startDate = int (floor (random.NextDouble() * 20.0 + 20.0)) * 100
        let turns = int (random.NextDouble() * 20.0 + 20.0)

        { Enterprise = enterprise
          Klingons = [||]
          CurrentQuadrant = createEmptySectorMap ()
          Quadrants = quadrants
          Stardate =
            { Current = startDate
              Start = startDate
              Turns = turns }
          QuadrantsScanned = Set.empty
          Random = random
          InitialKlingons = totalKlingons }

let initializeGame (seed: int) : GameState =
    let random = Random(seed) :> IRandomService
    let state = tryGenerateGame random
    let _, state = enterQuadrant state
    state
