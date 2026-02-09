module StarTrek.Enterprise

open StarTrek.GameTypes

let resetEnterprise (quadrant: Position) (sector: Position) : Enterprise =
    { Sector = sector
      Quadrant = quadrant
      Energy = 3000.0
      Shields = 0.0
      Torpedoes = 10
      Damage = GameDefaults.initialDeviceStatus }

let systemName (system: SystemDamage) : string =
    match system with
    | WarpEngines -> "WARP ENGINES"
    | ShortRangeSensors -> "SHORT RANGE SENSORS"
    | LongRangeSensors -> "LONG RANGE SENSORS"
    | Phasers -> "PHASER CONTROL"
    | PhotonTubes -> "PHOTON TUBES"
    | DamageControl -> "DAMAGE CONTROL"
    | ShieldControl -> "SHIELD CONTROL"
    | Computer -> "LIBRARY-COMPUTER"

let allSystems = [|
    WarpEngines; ShortRangeSensors; LongRangeSensors; Phasers;
    PhotonTubes; DamageControl; ShieldControl; Computer
|]

let getDamagedSystems (enterprise: Enterprise) : (string * int) list =
    enterprise.Damage
    |> List.filter (fun d -> d.Amount < 0)
    |> List.map (fun d -> systemName d.System, d.Amount)

let getAllDeviceStatuses (enterprise: Enterprise) : (string * int) list =
    enterprise.Damage
    |> List.map (fun d -> systemName d.System, d.Amount)

/// §5.2 Automatic Repair: each damaged device gets +1 per warp move
let automaticRepair (enterprise: Enterprise) : Enterprise =
    let newDamage =
        enterprise.Damage
        |> List.map (fun d ->
            if d.Amount < 0 then { d with Amount = d.Amount + 1 }
            else d)
    { enterprise with Damage = newDamage }

/// §5.3 Random Damage/Repair Events: 20% chance per warp move
let randomDamageEvent (random: IRandomService) (enterprise: Enterprise) : string list * Enterprise =
    if random.NextDouble() > 0.2 then
        [], enterprise
    else
        let deviceIndex = int (random.NextDouble() * 8.0)
        let deviceIndex = min deviceIndex 7  // clamp to valid range
        let targetSystem = allSystems.[deviceIndex]
        let severity = int (random.NextDouble() * 5.0) + 1
        let isImprovement = random.NextDouble() >= 0.5

        let newDamage =
            enterprise.Damage
            |> List.map (fun d ->
                if d.System = targetSystem then
                    if isImprovement then { d with Amount = d.Amount + severity }
                    else { d with Amount = d.Amount - severity }
                else d)

        let deviceName = systemName targetSystem
        let msg =
            if isImprovement then
                sprintf "DAMAGE CONTROL REPORT: %s STATE OF REPAIR IMPROVED" deviceName
            else
                sprintf "DAMAGE CONTROL REPORT: %s DAMAGED" deviceName

        [""; msg; ""], { enterprise with Damage = newDamage }

let isShieldControlDamaged (enterprise: Enterprise) : bool =
    enterprise.Damage
    |> List.exists (fun d -> d.System = ShieldControl && d.Amount < 0)

let isDamageControlDamaged (enterprise: Enterprise) : bool =
    enterprise.Damage
    |> List.exists (fun d -> d.System = DamageControl && d.Amount < 0)

let isShortRangeSensorsDamaged (enterprise: Enterprise) : bool =
    enterprise.Damage
    |> List.exists (fun d -> d.System = ShortRangeSensors && d.Amount < 0)

let isPhasersDamaged (enterprise: Enterprise) : bool =
    enterprise.Damage
    |> List.exists (fun d -> d.System = Phasers && d.Amount < 0)

let isPhotonTubesDamaged (enterprise: Enterprise) : bool =
    enterprise.Damage
    |> List.exists (fun d -> d.System = PhotonTubes && d.Amount < 0)

let isComputerDamaged (enterprise: Enterprise) : bool =
    enterprise.Damage
    |> List.exists (fun d -> d.System = Computer && d.Amount < 0)

let getPhaserRepairTime (enterprise: Enterprise) : int =
    enterprise.Damage
    |> List.tryFind (fun d -> d.System = Phasers)
    |> Option.map (fun d -> abs d.Amount)
    |> Option.defaultValue 0

/// §7.1/§7.2/§7.3 Spec hit formula: (baseEnergy / distance) × (2 × randomFactor)
let calculateHitDamage (from: Position) (target: Position) (baseEnergy: float) (randomFactor: float) : float =
    let dx = float (target.X - from.X)
    let dy = float (target.Y - from.Y)
    let distance = sqrt (dx * dx + dy * dy) |> max 1.0
    (baseEnergy / distance) * (2.0 * randomFactor)

let condition (enterprise: Enterprise) : ShipCondition =
    match enterprise.Shields, enterprise.Energy with
    | (s, _) when s < 0.0 -> Destroyed
    | (s, e) when s > 0.0 && s < 1.1 && e = 0.0 -> DeadInSpace
    | _ -> Operational

/// §7.3 Klingon Attack: hit = (klingon_shields / distance) × (2 × random)
let klingonAttack (state: GameState) : string list * GameState =
    if state.Klingons.Length = 0 then
        [], state
    else
        let folder (msgs, enterprise: Enterprise) (klingon: Klingon) =
            let randomFactor = state.Random.NextDouble()
            let damage = calculateHitDamage klingon.Sector enterprise.Sector klingon.Energy randomFactor
            let newShields = enterprise.Shields - damage
            let newEnterprise = { enterprise with Shields = newShields }
            let hitMsg = sprintf "%3.1f UNIT HIT ON ENTERPRISE FROM SECTOR %d,%d   (%3.1f LEFT)" damage klingon.Sector.X klingon.Sector.Y newShields
            (msgs @ [hitMsg], newEnterprise)

        let allMsgs, finalEnterprise =
            state.Klingons
            |> Array.fold folder ([], state.Enterprise)

        allMsgs, { state with Enterprise = finalEnterprise }

let firePhasers (blastEnergy: float) (state: GameState) : string list * GameState =
    let enterprise = state.Enterprise
    let newEnterprise = { enterprise with Energy = enterprise.Energy - blastEnergy }
    let numKlingons = state.Klingons.Length

    let perKlingon, overloadMsgs =
        if blastEnergy > 1090.0 then
            9.0, ["PHASER OVERLOAD! ENERGY DISPERSED ACROSS ALL TARGETS."]
        else
            blastEnergy / float numKlingons, []

    let computerDamaged = isComputerDamaged enterprise

    let folder (msgs, klingons, sectorMap, quadrants: Quadrant[,]) (klingon: Klingon) =
        let effectiveEnergy =
            if computerDamaged then perKlingon * state.Random.NextDouble()
            else perKlingon
        let randomFactor = state.Random.NextDouble()
        let damage = calculateHitDamage enterprise.Sector klingon.Sector effectiveEnergy randomFactor

        let newEnergy = klingon.Energy - damage
        let hitMsg = sprintf "%.0f UNIT HIT ON KLINGON AT SECTOR %d,%d" damage klingon.Sector.X klingon.Sector.Y

        if newEnergy <= 0.0 then
            let destroyMsg = sprintf "*** KLINGON AT SECTOR %d,%d DESTROYED ***" klingon.Sector.X klingon.Sector.Y
            let newSectorMap = Array2D.copy sectorMap
            newSectorMap.[klingon.Sector.Y - 1, klingon.Sector.X - 1] <- Empty
            let qx = state.Enterprise.Quadrant.X - 1
            let qy = state.Enterprise.Quadrant.Y - 1
            let newQuadrants = Array2D.copy quadrants
            let (q: Quadrant) = newQuadrants.[qx, qy]
            newQuadrants.[qx, qy] <- { q with Klingons = q.Klingons - 1 }
            (msgs @ [hitMsg; destroyMsg], klingons, newSectorMap, newQuadrants)
        else
            let remainMsg = sprintf "(SENSORS SHOW %.0f UNITS REMAINING)" newEnergy
            let updatedKlingon = { klingon with Energy = newEnergy }
            (msgs @ [hitMsg; remainMsg], klingons @ [updatedKlingon], sectorMap, quadrants)

    let allMsgs, survivingKlingons, finalSectorMap, finalQuadrants =
        state.Klingons
        |> Array.fold folder (overloadMsgs, [], state.CurrentQuadrant, state.Quadrants)

    let newState =
        { state with
            Enterprise = newEnterprise
            Klingons = survivingKlingons |> List.toArray
            CurrentQuadrant = finalSectorMap
            Quadrants = finalQuadrants }

    allMsgs, newState

let transferShields (requested: float) (enterprise: Enterprise) : Result<Enterprise, string> =
    let total = enterprise.Energy + enterprise.Shields
    if requested < 0.0 then
        Error "INVALID. SHIELDS UNCHANGED"
    elif requested > total then
        Error "SHIELD CONTROL REPORTS: 'THIS IS NOT THE FEDERATION TREASURY.'\nSHIELDS UNCHANGED"
    else
        Ok { enterprise with Shields = requested; Energy = total - requested }

let firePhotonTorpedo (direction: float * float) (state: GameState) : string list * GameState =
    let enterprise = state.Enterprise
    let newEnterprise = { enterprise with Torpedoes = enterprise.Torpedoes - 1 }
    let dx, dy = direction
    let startX = float enterprise.Sector.X
    let startY = float enterprise.Sector.Y

    let rec trace (fx: float) (fy: float) (msgs: string list) (currentState: GameState) =
        let nx = fx + dx
        let ny = fy + dy
        let sx = int (System.Math.Round(nx))
        let sy = int (System.Math.Round(ny))

        if sx < 1 || sx > 8 || sy < 1 || sy > 8 then
            msgs @ ["TORPEDO MISSED"], currentState
        else
            let trackMsg = sprintf "TORPEDO TRACK: %d,%d" sx sy
            let msgs = msgs @ [trackMsg]
            match currentState.CurrentQuadrant.[sy - 1, sx - 1] with
            | Empty -> trace nx ny msgs currentState
            | Klingon _ ->
                let damage = float (280 + state.Random.Next(100))
                let hitKlingon =
                    currentState.Klingons
                    |> Array.tryFind (fun k -> k.Sector.X = sx && k.Sector.Y = sy)
                match hitKlingon with
                | Some klingon ->
                    let newEnergy = klingon.Energy - damage
                    if newEnergy <= 0.0 then
                        let destroyMsg = sprintf "*** KLINGON AT SECTOR %d,%d DESTROYED ***" sx sy
                        let newSectorMap = Array2D.copy currentState.CurrentQuadrant
                        newSectorMap.[sy - 1, sx - 1] <- Empty
                        let qx = currentState.Enterprise.Quadrant.X - 1
                        let qy = currentState.Enterprise.Quadrant.Y - 1
                        let newQuadrants = Array2D.copy currentState.Quadrants
                        let q = newQuadrants.[qx, qy]
                        newQuadrants.[qx, qy] <- { q with Klingons = q.Klingons - 1 }
                        let survivingKlingons =
                            currentState.Klingons
                            |> Array.filter (fun k -> k.Sector <> klingon.Sector)
                        let newState =
                            { currentState with
                                Klingons = survivingKlingons
                                CurrentQuadrant = newSectorMap
                                Quadrants = newQuadrants }
                        msgs @ [destroyMsg], newState
                    else
                        let remainMsg = sprintf "(SENSORS SHOW %.0f UNITS REMAINING)" newEnergy
                        let updatedKlingons =
                            currentState.Klingons
                            |> Array.map (fun k ->
                                if k.Sector.X = sx && k.Sector.Y = sy then { k with Energy = newEnergy }
                                else k)
                        let newSectorMap = Array2D.copy currentState.CurrentQuadrant
                        newSectorMap.[sy - 1, sx - 1] <- Klingon newEnergy
                        let newState =
                            { currentState with
                                Klingons = updatedKlingons
                                CurrentQuadrant = newSectorMap }
                        msgs @ [remainMsg], newState
                | None -> trace nx ny msgs currentState
            | Starbase ->
                let newSectorMap = Array2D.copy currentState.CurrentQuadrant
                newSectorMap.[sy - 1, sx - 1] <- Empty
                let qx = currentState.Enterprise.Quadrant.X - 1
                let qy = currentState.Enterprise.Quadrant.Y - 1
                let newQuadrants = Array2D.copy currentState.Quadrants
                let q = newQuadrants.[qx, qy]
                newQuadrants.[qx, qy] <- { q with Starbases = q.Starbases - 1 }
                let newState =
                    { currentState with
                        CurrentQuadrant = newSectorMap
                        Quadrants = newQuadrants }
                msgs @ ["*** STAR BASE DESTROYED ***  .......CONGRATULATIONS"], newState
            | Star ->
                msgs @ ["YOU CAN'T DESTROY STARS SILLY"], currentState
            | Enterprise -> trace nx ny msgs currentState

    let stateWithTorpedo = { state with Enterprise = newEnterprise }
    trace startX startY [] stateWithTorpedo
