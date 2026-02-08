module StarTrek.Enterprise

open StarTrek.GameTypes

let resetEnterprise (quadrant: Position) (sector: Position) : Enterprise =
    { Sector = sector
      Quadrant = quadrant
      Energy = 3000.0
      Shields = 0.0
      Torpedoes = 10
      Damage = GameDefaults.initialDeviceStatus }

let isShieldControlDamaged (enterprise: Enterprise) : bool =
    enterprise.Damage
    |> List.exists (fun d -> d.System = ShieldControl && d.Amount < 0)

let isPhasersDamaged (enterprise: Enterprise) : bool =
    enterprise.Damage
    |> List.exists (fun d -> d.System = Phasers && d.Amount < 0)

let isComputerDamaged (enterprise: Enterprise) : bool =
    enterprise.Damage
    |> List.exists (fun d -> d.System = Computer && d.Amount < 0)

let getPhaserRepairTime (enterprise: Enterprise) : int =
    enterprise.Damage
    |> List.tryFind (fun d -> d.System = Phasers)
    |> Option.map (fun d -> abs d.Amount)
    |> Option.defaultValue 0

let calculatePhaserDamage (from: Position) (target: Position) (blastEnergy: float) : float =
    let dx = target.X - from.X
    let dy = target.Y - from.Y
    blastEnergy * 30.0 / (30.0 + float (dx * dx + dy * dy) + 1.0)

let condition (enterprise: Enterprise) : ShipCondition =
    match enterprise.Shields, enterprise.Energy with
    | (s, _) when s < 0.0 -> Destroyed
    | (s, e) when s > 0.0 && s < 1.1 && e = 0.0 -> DeadInSpace
    | _ -> Operational

let klingonAttack (state: GameState) : string list * GameState =
    if state.Klingons.Length = 0 then
        [], state
    else
        let folder (msgs, enterprise: Enterprise) (klingon: Klingon) =
            let blastEnergy = klingon.Energy * 2.0
            let damage = calculatePhaserDamage enterprise.Sector klingon.Sector blastEnergy
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
        let baseDamage = calculatePhaserDamage enterprise.Sector klingon.Sector perKlingon
        let damage =
            if computerDamaged then baseDamage * state.Random.NextDouble()
            else baseDamage

        let newEnergy = klingon.Energy - damage
        let hitMsg = sprintf "%.0f UNIT HIT ON KLINGON AT SECTOR %d,%d" damage klingon.Sector.X klingon.Sector.Y

        if newEnergy <= 0.0 then
            let destroyMsg = "*** KLINGON DESTROYED ***"
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
