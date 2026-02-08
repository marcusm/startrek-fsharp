module StarTrek.Galaxy

open StarTrek.GameTypes
open StarTrek.Enterprise

let galaxySize = 8

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
    int (warpFactor * 8.0 + 0.5) + 10

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

let enterQuadrant (state: GameState) : GameState =
    let qx = state.Enterprise.Quadrant.X - 1
    let qy = state.Enterprise.Quadrant.Y - 1
    let quadrant = state.Quadrants.[qx, qy]
    let random = state.Random

    let enterprisePos = state.Enterprise.Sector
    let occupied = Set.singleton enterprisePos

    let klingonPositions, occupied = placeRandomItems random quadrant.Klingons occupied
    let starbasePositions, occupied = placeRandomItems random quadrant.Starbases occupied
    let starPositions, _ = placeRandomItems random quadrant.Stars occupied

    let klingons =
        klingonPositions
        |> List.map (fun pos -> { Sector = pos; Energy = 200.0 })
        |> List.toArray

    let positionMap =
        [ yield (enterprisePos, Enterprise)
          for pos in klingonPositions do yield (pos, Klingon 200.0)
          for pos in starbasePositions do yield (pos, Starbase)
          for pos in starPositions do yield (pos, Star) ]
        |> Map.ofList

    let sectorMap =
        Array2D.init galaxySize galaxySize (fun row col ->
            let pos = { X = col + 1; Y = row + 1 }
            match Map.tryFind pos positionMap with
            | Some sector -> sector
            | None -> Empty)

    let scanned = Set.add state.Enterprise.Quadrant state.QuadrantsScanned
    { state with CurrentQuadrant = sectorMap; Klingons = klingons; QuadrantsScanned = scanned }

let executeWarp (direction: float * float) (warpFactor: float) (state: GameState) : string list * GameState =
    let numSteps = int (warpFactor * 8.0 + 0.5)
    let energyCost = float (warpEnergyCost warpFactor)

    if state.Enterprise.Energy - state.Enterprise.Shields < energyCost then
        ["ENGINEERING REPORTS: 'INSUFFICIENT ENERGY AVAILABLE"; sprintf "                      FOR MANEUVERING AT WARP %g'!" warpFactor], state
    else
        let dx, dy = direction
        let sectorMap = Array2D.copy state.CurrentQuadrant
        let ep = state.Enterprise.Sector
        sectorMap.[ep.Y - 1, ep.X - 1] <- Empty

        let startX = float ep.X - 0.5
        let startY = float ep.Y - 0.5

        let rec walk step (fx: float) (fy: float) (prevX: int) (prevY: int) =
            if step > numSteps then
                (prevX, prevY, false, false)
            else
                let nx = fx + dx
                let ny = fy + dy
                let sx = int (System.Math.Round(nx))
                let sy = int (System.Math.Round(ny))
                if sx < 1 || sx > galaxySize || sy < 1 || sy > galaxySize then
                    (prevX, prevY, true, false)
                else
                    match sectorMap.[sy - 1, sx - 1] with
                    | Empty -> walk (step + 1) nx ny sx sy
                    | _ -> (prevX, prevY, false, true)

        let (finalX, finalY, exited, blocked) = walk 1 startX startY ep.X ep.Y

        let msgs = if blocked then ["WARP ENGINES SHUT DOWN AT SECTOR"; sprintf "%d,%d DUE TO BAD NAVIGATION" finalX finalY] else []

        if exited then
            let absX = float (state.Enterprise.Quadrant.X - 1) * 8.0 + startX + dx * float numSteps
            let absY = float (state.Enterprise.Quadrant.Y - 1) * 8.0 + startY + dy * float numSteps
            let newQX = int (System.Math.Floor(absX / 8.0)) + 1
            let newQY = int (System.Math.Floor(absY / 8.0)) + 1
            let clampedQX = max 1 (min galaxySize newQX)
            let clampedQY = max 1 (min galaxySize newQY)
            let hitEdge = clampedQX <> newQX || clampedQY <> newQY

            let edgeMsgs =
                if hitEdge then
                    ["LT. UHURA REPORTS MESSAGE FROM STARFLEET COMMAND:";
                     "  'PERMISSION TO ATTEMPT CROSSING OF GALACTIC PERIMETER";
                     "  IS HEREBY *DENIED*. SHUT DOWN YOUR ENGINES.'";
                     "CHIEF ENGINEER SCOTT REPORTS 'WARP ENGINES SHUT DOWN";
                     sprintf "  AT SECTOR %d,%d OF QUADRANT %d,%d.'" finalX finalY clampedQX clampedQY]
                else []

            let newSectorPos = findEmptyPosition state.Random Set.empty

            let newEnterprise =
                { state.Enterprise with
                    Quadrant = { X = clampedQX; Y = clampedQY }
                    Sector = newSectorPos
                    Energy = state.Enterprise.Energy - energyCost }

            let newState =
                { state with
                    Enterprise = newEnterprise
                    Stardate = { state.Stardate with Current = state.Stardate.Current + 1 } }

            let newState = enterQuadrant newState
            edgeMsgs, newState
        else
            sectorMap.[finalY - 1, finalX - 1] <- Enterprise
            let newEnterprise =
                { state.Enterprise with
                    Sector = { X = finalX; Y = finalY }
                    Energy = state.Enterprise.Energy - energyCost }

            let newState =
                { state with
                    Enterprise = newEnterprise
                    CurrentQuadrant = sectorMap
                    Stardate = { state.Stardate with Current = state.Stardate.Current + 1 } }

            msgs, newState

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
    random.Next(4, 7)

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

        let startDate = int (((random.NextDouble() * 20.0) + 20.0) * 100.0)
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
          Random = random }

let initializeGame (seed: int) : GameState =
    let random = Random(seed) :> IRandomService
    let state = tryGenerateGame random
    enterQuadrant state
