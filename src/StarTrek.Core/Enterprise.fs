module StarTrek.Enterprise

open StarTrek.GameTypes

let resetEnterprise (quadrant: Position) (sector: Position) : Enterprise =
    { Sector = sector
      Quadrant = quadrant
      Energy = 3000.0
      Shields = 0.0
      Torpedoes = 10
      Damage = GameDefaults.initialDeviceStatus }
