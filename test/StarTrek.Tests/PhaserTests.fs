module StarTrek.Tests.PhaserTests

open Expecto
open StarTrek.GameTypes
open StarTrek.Enterprise

type FixedRandom(value: float) =
    interface IRandomService with
        member _.Next() = 0
        member _.Next(_max: int) = 0
        member _.Next(min: int, _max: int) = min
        member _.NextDouble() = value

let private mkKlingon x y energy : Klingon =
    { Sector = { X = x; Y = y }; Energy = energy }

let private makeStateWithShields (klingons: Klingon array) enterpriseSector shields =
    let enterprise = { resetEnterprise { X = 4; Y = 4 } enterpriseSector with Shields = shields }
    let sectorMap = Array2D.create 8 8 Empty
    sectorMap.[enterpriseSector.Y - 1, enterpriseSector.X - 1] <- Sector.Enterprise
    for k in klingons do
        sectorMap.[k.Sector.Y - 1, k.Sector.X - 1] <- Klingon k.Energy
    let quadrants = Array2D.init 8 8 (fun x y ->
        { Quadrant = { X = x + 1; Y = y + 1 }; Klingons = 0; Starbases = 0; Stars = 4 })
    let qx = enterprise.Quadrant.X - 1
    let qy = enterprise.Quadrant.Y - 1
    quadrants.[qx, qy] <- { quadrants.[qx, qy] with Klingons = Array.length klingons }
    { Enterprise = enterprise
      Klingons = klingons
      CurrentQuadrant = sectorMap
      Quadrants = quadrants
      Stardate = { Current = 2500; Start = 2500; Turns = 30 }
      QuadrantsScanned = Set.empty
      Random = FixedRandom(1.0) :> IRandomService
      InitialKlingons = Array.length klingons }

let private makeState (klingons: Klingon array) enterpriseSector =
    let enterprise = resetEnterprise { X = 4; Y = 4 } enterpriseSector
    let sectorMap = Array2D.create 8 8 Empty
    sectorMap.[enterpriseSector.Y - 1, enterpriseSector.X - 1] <- Sector.Enterprise
    for k in klingons do
        sectorMap.[k.Sector.Y - 1, k.Sector.X - 1] <- Klingon k.Energy
    let quadrants = Array2D.init 8 8 (fun x y ->
        { Quadrant = { X = x + 1; Y = y + 1 }; Klingons = 0; Starbases = 0; Stars = 4 })
    let qx = enterprise.Quadrant.X - 1
    let qy = enterprise.Quadrant.Y - 1
    quadrants.[qx, qy] <- { quadrants.[qx, qy] with Klingons = Array.length klingons }
    { Enterprise = enterprise
      Klingons = klingons
      CurrentQuadrant = sectorMap
      Quadrants = quadrants
      Stardate = { Current = 2500; Start = 2500; Turns = 30 }
      QuadrantsScanned = Set.empty
      Random = FixedRandom(1.0) :> IRandomService
      InitialKlingons = Array.length klingons }

[<Tests>]
let phaserDamageCheckTests =
    testList "Phaser damage checks" [
        testCase "isPhasersDamaged returns false when undamaged" <| fun _ ->
            let ship = resetEnterprise { X = 4; Y = 4 } { X = 1; Y = 1 }
            Expect.isFalse (isPhasersDamaged ship) "should not be damaged"

        testCase "isPhasersDamaged returns true when damaged" <| fun _ ->
            let ship = resetEnterprise { X = 4; Y = 4 } { X = 1; Y = 1 }
            let damaged = { ship with Damage = ship.Damage |> List.map (fun d -> if d.System = Phasers then { d with Amount = -3 } else d) }
            Expect.isTrue (isPhasersDamaged damaged) "should be damaged"

        testCase "isComputerDamaged returns false when undamaged" <| fun _ ->
            let ship = resetEnterprise { X = 4; Y = 4 } { X = 1; Y = 1 }
            Expect.isFalse (isComputerDamaged ship) "should not be damaged"

        testCase "isComputerDamaged returns true when damaged" <| fun _ ->
            let ship = resetEnterprise { X = 4; Y = 4 } { X = 1; Y = 1 }
            let damaged = { ship with Damage = ship.Damage |> List.map (fun d -> if d.System = Computer then { d with Amount = -2 } else d) }
            Expect.isTrue (isComputerDamaged damaged) "should be damaged"

        testCase "getPhaserRepairTime returns abs of damage amount" <| fun _ ->
            let ship = resetEnterprise { X = 4; Y = 4 } { X = 1; Y = 1 }
            let damaged = { ship with Damage = ship.Damage |> List.map (fun d -> if d.System = Phasers then { d with Amount = -5 } else d) }
            Expect.equal (getPhaserRepairTime damaged) 5 "repair time should be 5"

        testCase "getPhaserRepairTime returns 0 when undamaged" <| fun _ ->
            let ship = resetEnterprise { X = 4; Y = 4 } { X = 1; Y = 1 }
            Expect.equal (getPhaserRepairTime ship) 0 "repair time should be 0"
    ]

[<Tests>]
let firePhaserTests =
    testList "firePhasers" [
        testCase "deducts blast energy from Enterprise" <| fun _ ->
            let state = makeState [| mkKlingon 3 4 200.0 |] { X = 4; Y = 4 }
            let _, newState = firePhasers 500.0 state
            Expect.equal newState.Enterprise.Energy 2500.0 "energy should be 3000 - 500"

        testCase "single Klingon adjacent gets high damage" <| fun _ ->
            let state = makeState [| mkKlingon 4 5 2000.0 |] { X = 4; Y = 4 }
            // distance=1, perKlingon=500, randomFactor=1.0
            // damage = (500/1) * 2 = 1000
            let msgs, _ = firePhasers 500.0 state
            Expect.isTrue (msgs.[0].Contains("1000 UNIT HIT")) "should have high damage at distance 1"

        testCase "Klingon at distance takes reduced damage" <| fun _ ->
            // Enterprise at 4,4, Klingon at 4,7 -> distance=3
            let state = makeState [| mkKlingon 4 7 500.0 |] { X = 4; Y = 4 }
            // perKlingon=300, damage = (300/3) * 2 = 200
            let msgs, _ = firePhasers 300.0 state
            Expect.isTrue (msgs.[0].Contains("200 UNIT HIT")) "should have reduced damage at distance"

        testCase "Klingon destroyed when energy drops to zero" <| fun _ ->
            let state = makeState [| mkKlingon 4 5 50.0 |] { X = 4; Y = 4 }
            let msgs, newState = firePhasers 500.0 state
            Expect.equal newState.Klingons.Length 0 "klingon should be removed"
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("KLINGON DESTROYED"))) "should report destruction"
            Expect.equal newState.CurrentQuadrant.[4, 3] Empty "sector should be empty"

        testCase "destroyed Klingon decrements quadrant count" <| fun _ ->
            let state = makeState [| mkKlingon 4 5 50.0 |] { X = 4; Y = 4 }
            let _, newState = firePhasers 500.0 state
            let qx = newState.Enterprise.Quadrant.X - 1
            let qy = newState.Enterprise.Quadrant.Y - 1
            Expect.equal newState.Quadrants.[qx, qy].Klingons 0 "quadrant klingon count should be 0"

        testCase "surviving Klingon shows remaining energy" <| fun _ ->
            let state = makeState [| mkKlingon 4 6 1000.0 |] { X = 4; Y = 4 }
            // distance=2, perKlingon=500, damage = (500/2)*2 = 500, surviving=500
            let msgs, newState = firePhasers 500.0 state
            Expect.equal newState.Klingons.Length 1 "klingon should survive"
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("SENSORS SHOW"))) "should show remaining"

        testCase "multiple Klingons split energy evenly" <| fun _ ->
            let state = makeState [| mkKlingon 5 4 500.0; mkKlingon 6 4 500.0 |] { X = 4; Y = 4 }
            // perKlingon = 200/2 = 100
            // K1 distance=1: (100/1)*2=200, K2 distance=2: (100/2)*2=100
            let msgs, _ = firePhasers 200.0 state
            let hitMsgs = msgs |> List.filter (fun m -> m.Contains("UNIT HIT"))
            Expect.equal hitMsgs.Length 2 "should hit both Klingons"

        testCase "overload above 1090 sets per-Klingon to 9" <| fun _ ->
            let state = makeState [| mkKlingon 4 5 200.0 |] { X = 4; Y = 4 }
            // perKlingon = 9, distance=1, damage = (9/1)*2 = 18
            let msgs, _ = firePhasers 1100.0 state
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("OVERLOAD"))) "should report overload"
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("18 UNIT HIT"))) "damage should be ~18"

        testCase "computer damage reduces hit effectiveness" <| fun _ ->
            let state = makeState [| mkKlingon 4 5 5000.0 |] { X = 4; Y = 4 }
            let damagedEnterprise =
                { state.Enterprise with
                    Damage = state.Enterprise.Damage |> List.map (fun d ->
                        if d.System = Computer then { d with Amount = -2 } else d) }
            // FixedRandom(0.5): effectiveEnergy=500*0.5=250, randomFactor=0.5
            // damage = (250/1) * 2 * 0.5 = 250
            let stateWithDamagedComputer =
                { state with
                    Enterprise = damagedEnterprise
                    Random = FixedRandom(0.5) :> IRandomService }
            let msgs1, _ = firePhasers 500.0 state
            let msgs2, _ = firePhasers 500.0 stateWithDamagedComputer
            let getDamage (msgs: string list) =
                let hitMsg = msgs |> List.find (fun m -> m.Contains("UNIT HIT"))
                let parts = hitMsg.Split(' ')
                float parts.[0]
            let normalDamage = getDamage msgs1
            let reducedDamage = getDamage msgs2
            Expect.isTrue (reducedDamage < normalDamage) "computer damage should reduce hit"
    ]

[<Tests>]
let calculateHitDamageTests =
    testList "calculateHitDamage" [
        testCase "adjacent position gives maximum damage" <| fun _ ->
            // distance=1, damage = (300/1) * 2 * 1.0 = 600
            let damage = calculateHitDamage { X = 4; Y = 4 } { X = 4; Y = 5 } 300.0 1.0
            Expect.floatClose Accuracy.medium damage 600.0 "max damage at distance 1"

        testCase "distance reduces damage" <| fun _ ->
            // distance=3, damage = (300/3) * 2 * 1.0 = 200
            let damage = calculateHitDamage { X = 4; Y = 4 } { X = 4; Y = 7 } 300.0 1.0
            Expect.floatClose Accuracy.medium damage 200.0 "reduced damage at distance 3"

        testCase "random factor scales damage" <| fun _ ->
            // distance=1, damage = (300/1) * 2 * 0.5 = 300
            let damage = calculateHitDamage { X = 4; Y = 4 } { X = 4; Y = 5 } 300.0 0.5
            Expect.floatClose Accuracy.medium damage 300.0 "half random gives half max damage"

        testCase "large distance gives low damage" <| fun _ ->
            // distance=7, damage = (400/7) * 2 * 1.0 = 114.29
            let damage = calculateHitDamage { X = 1; Y = 1 } { X = 8; Y = 1 } 400.0 1.0
            Expect.floatClose Accuracy.medium damage (800.0 / 7.0) "low damage at large distance"

        testCase "same position clamps distance to 1" <| fun _ ->
            // distance=0 clamped to 1, damage = (400/1) * 2 * 1.0 = 800
            let damage = calculateHitDamage { X = 4; Y = 4 } { X = 4; Y = 4 } 400.0 1.0
            Expect.floatClose Accuracy.medium damage 800.0 "same position uses minimum distance 1"
    ]

[<Tests>]
let conditionTests =
    testList "condition" [
        testCase "Destroyed when shields below zero" <| fun _ ->
            let ship = { resetEnterprise { X = 4; Y = 4 } { X = 1; Y = 1 } with Shields = -10.0 }
            Expect.equal (condition ship) Destroyed "should be destroyed"

        testCase "DeadInSpace when shields near zero and no energy" <| fun _ ->
            let ship = { resetEnterprise { X = 4; Y = 4 } { X = 1; Y = 1 } with Shields = 0.5; Energy = 0.0 }
            Expect.equal (condition ship) DeadInSpace "should be dead in space"

        testCase "Operational with normal shields and energy" <| fun _ ->
            let ship = { resetEnterprise { X = 4; Y = 4 } { X = 1; Y = 1 } with Shields = 500.0 }
            Expect.equal (condition ship) Operational "should be operational"

        testCase "Operational with zero shields but some energy" <| fun _ ->
            let ship = resetEnterprise { X = 4; Y = 4 } { X = 1; Y = 1 }
            Expect.equal (condition ship) Operational "should be operational with no shields"
    ]

[<Tests>]
let klingonAttackTests =
    testList "klingonAttack" [
        testCase "no Klingons returns empty messages and unchanged state" <| fun _ ->
            let state = makeStateWithShields [||] { X = 4; Y = 4 } 500.0
            let msgs, newState = klingonAttack state
            Expect.isEmpty msgs "should have no messages"
            Expect.equal newState.Enterprise.Shields 500.0 "shields should be unchanged"

        testCase "single Klingon deals damage to shields" <| fun _ ->
            // Klingon(4,5) with 200 energy, Enterprise at (4,4)
            // distance=1, randomFactor=1.0, damage = (200/1) * 2 = 400
            let state = makeStateWithShields [| mkKlingon 4 5 200.0 |] { X = 4; Y = 4 } 500.0
            let msgs, newState = klingonAttack state
            Expect.equal msgs.Length 1 "should have one hit message"
            Expect.floatClose Accuracy.medium newState.Enterprise.Shields 100.0 "shields should be 500 - 400"

        testCase "multiple Klingons deal cumulative damage" <| fun _ ->
            // Two Klingons at distance 1, each damage = 400
            let state = makeStateWithShields [| mkKlingon 5 4 200.0; mkKlingon 3 4 200.0 |] { X = 4; Y = 4 } 1000.0
            let msgs, newState = klingonAttack state
            Expect.equal msgs.Length 2 "should have two hit messages"
            Expect.floatClose Accuracy.medium newState.Enterprise.Shields 200.0 "shields reduced by cumulative damage"

        testCase "hit message format is correct" <| fun _ ->
            let state = makeStateWithShields [| mkKlingon 3 5 200.0 |] { X = 4; Y = 4 } 500.0
            let msgs, _ = klingonAttack state
            Expect.equal msgs.Length 1 "should have one message"
            Expect.isTrue (msgs.[0].Contains("UNIT HIT ON ENTERPRISE FROM SECTOR 3,5")) "should contain hit description"
            Expect.isTrue (msgs.[0].Contains("LEFT)")) "should contain remaining shields"

        testCase "Enterprise destruction when shields insufficient" <| fun _ ->
            // Klingon(4,5) with 200 energy -> distance=1, damage = 400
            // Shields = 100, so shields go to -300 -> Destroyed
            let state = makeStateWithShields [| mkKlingon 4 5 200.0 |] { X = 4; Y = 4 } 100.0
            let _, newState = klingonAttack state
            Expect.isTrue (newState.Enterprise.Shields < 0.0) "shields should be negative"
            Expect.equal (condition newState.Enterprise) Destroyed "should be destroyed"

        testCase "Klingon at distance deals less damage" <| fun _ ->
            // Enterprise at 4,4, Klingon at 4,7 -> distance=3
            // damage = (200/3) * 2 = 400/3 ≈ 133.33
            let state = makeStateWithShields [| mkKlingon 4 7 200.0 |] { X = 4; Y = 4 } 500.0
            let _, newState = klingonAttack state
            Expect.floatClose Accuracy.medium newState.Enterprise.Shields (500.0 - 400.0 / 3.0) "shields should reflect distance-based damage"
    ]
