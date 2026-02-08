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

let transferShields (requested: float) (enterprise: Enterprise) : Result<Enterprise, string> =
    let total = enterprise.Energy + enterprise.Shields
    if requested < 0.0 then
        Error "INVALID. SHIELDS UNCHANGED"
    elif requested > total then
        Error "SHIELD CONTROL REPORTS: 'THIS IS NOT THE FEDERATION TREASURY.'\nSHIELDS UNCHANGED"
    else
        Ok { enterprise with Shields = requested; Energy = total - requested }
