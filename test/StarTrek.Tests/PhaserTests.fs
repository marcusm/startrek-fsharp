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
      Random = FixedRandom(1.0) :> IRandomService }

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
      Random = FixedRandom(1.0) :> IRandomService }

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

        testCase "single Klingon same sector gets maximum damage" <| fun _ ->
            let state = makeState [| mkKlingon 4 4 200.0 |] { X = 4; Y = 4 }
            // dx=0, dy=0, damage = 500 * 30 / (30 + 0 + 1) = 500 * 30/31 ~= 483.87
            let msgs, _ = firePhasers 500.0 state
            Expect.isTrue (msgs.[0].Contains("484 UNIT HIT")) "should have high damage at distance 0"

        testCase "Klingon at distance takes reduced damage" <| fun _ ->
            // Enterprise at 1,1, Klingon at 4,4 -> dx=3, dy=3 -> dx^2+dy^2=18
            let state = makeState [| mkKlingon 4 4 200.0 |] { X = 1; Y = 1 }
            // damage = 500 * 30 / (30 + 18 + 1) = 500 * 30/49 ~= 306.12
            let msgs, _ = firePhasers 500.0 state
            Expect.isTrue (msgs.[0].Contains("306 UNIT HIT")) "should have reduced damage at distance"

        testCase "Klingon destroyed when energy drops to zero" <| fun _ ->
            let state = makeState [| mkKlingon 4 4 50.0 |] { X = 4; Y = 4 }
            let msgs, newState = firePhasers 500.0 state
            Expect.equal newState.Klingons.Length 0 "klingon should be removed"
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("KLINGON DESTROYED"))) "should report destruction"
            Expect.equal newState.CurrentQuadrant.[3, 3] Empty "sector should be empty"

        testCase "destroyed Klingon decrements quadrant count" <| fun _ ->
            let state = makeState [| mkKlingon 4 4 50.0 |] { X = 4; Y = 4 }
            let _, newState = firePhasers 500.0 state
            let qx = newState.Enterprise.Quadrant.X - 1
            let qy = newState.Enterprise.Quadrant.Y - 1
            Expect.equal newState.Quadrants.[qx, qy].Klingons 0 "quadrant klingon count should be 0"

        testCase "surviving Klingon shows remaining energy" <| fun _ ->
            let state = makeState [| mkKlingon 4 4 1000.0 |] { X = 4; Y = 4 }
            let msgs, newState = firePhasers 500.0 state
            Expect.equal newState.Klingons.Length 1 "klingon should survive"
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("SENSORS SHOW"))) "should show remaining"

        testCase "multiple Klingons split energy evenly" <| fun _ ->
            let state = makeState [| mkKlingon 4 4 200.0; mkKlingon 4 4 200.0 |] { X = 4; Y = 4 }
            // perKlingon = 600/2 = 300, damage = 300 * 30/31 ~= 290.32
            let msgs, _ = firePhasers 600.0 state
            let hitMsgs = msgs |> List.filter (fun m -> m.Contains("UNIT HIT"))
            Expect.equal hitMsgs.Length 2 "should hit both Klingons"

        testCase "overload above 1090 sets per-Klingon to 9" <| fun _ ->
            let state = makeState [| mkKlingon 4 4 200.0 |] { X = 4; Y = 4 }
            // perKlingon = 9, damage = 9 * 30/(30+0+1) = 9*30/31 ~= 8.71
            let msgs, _ = firePhasers 1100.0 state
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("OVERLOAD"))) "should report overload"
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("9 UNIT HIT"))) "damage should be ~9"

        testCase "computer damage reduces hit effectiveness" <| fun _ ->
            let state = makeState [| mkKlingon 4 4 200.0 |] { X = 4; Y = 4 }
            let damagedEnterprise =
                { state.Enterprise with
                    Damage = state.Enterprise.Damage |> List.map (fun d ->
                        if d.System = Computer then { d with Amount = -2 } else d) }
            // FixedRandom returns 0.5 for NextDouble, so damage = baseDamage * 0.5
            let stateWithDamagedComputer =
                { state with
                    Enterprise = damagedEnterprise
                    Random = FixedRandom(0.5) :> IRandomService }
            let msgs1, _ = firePhasers 500.0 state
            let msgs2, _ = firePhasers 500.0 stateWithDamagedComputer
            // With computer damaged and random=0.5, damage should be half
            let getDamage (msgs: string list) =
                let hitMsg = msgs |> List.find (fun m -> m.Contains("UNIT HIT"))
                let parts = hitMsg.Split(' ')
                float parts.[0]
            let normalDamage = getDamage msgs1
            let reducedDamage = getDamage msgs2
            Expect.isTrue (reducedDamage < normalDamage) "computer damage should reduce hit"
            Expect.floatClose Accuracy.medium reducedDamage (normalDamage * 0.5) "should be about half"
    ]

[<Tests>]
let calculatePhaserDamageTests =
    testList "calculatePhaserDamage" [
        testCase "same position gives maximum damage" <| fun _ ->
            let damage = calculatePhaserDamage { X = 4; Y = 4 } { X = 4; Y = 4 } 400.0
            // 400 * 30 / (30 + 0 + 1) = 400 * 30/31
            Expect.floatClose Accuracy.medium damage (400.0 * 30.0 / 31.0) "max damage at distance 0"

        testCase "distance reduces damage" <| fun _ ->
            // dx=3, dy=3 -> dx^2+dy^2=18
            let damage = calculatePhaserDamage { X = 1; Y = 1 } { X = 4; Y = 4 } 400.0
            // 400 * 30 / (30 + 18 + 1) = 400 * 30/49
            Expect.floatClose Accuracy.medium damage (400.0 * 30.0 / 49.0) "reduced damage at distance"

        testCase "large distance gives very low damage" <| fun _ ->
            // dx=7, dy=7 -> dx^2+dy^2=98
            let damage = calculatePhaserDamage { X = 1; Y = 1 } { X = 8; Y = 8 } 400.0
            // 400 * 30 / (30 + 98 + 1) = 400 * 30/129
            Expect.floatClose Accuracy.medium damage (400.0 * 30.0 / 129.0) "low damage at large distance"
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
            // Klingon at 4,4 with 200 energy, Enterprise at 4,4
            // blastEnergy = 200 * 2 = 400, damage = 400 * 30 / (30+0+1) = 400*30/31
            let state = makeStateWithShields [| mkKlingon 4 4 200.0 |] { X = 4; Y = 4 } 500.0
            let msgs, newState = klingonAttack state
            let expectedDamage = 400.0 * 30.0 / 31.0
            Expect.equal msgs.Length 1 "should have one hit message"
            Expect.floatClose Accuracy.medium newState.Enterprise.Shields (500.0 - expectedDamage) "shields should be reduced"

        testCase "multiple Klingons deal cumulative damage" <| fun _ ->
            // Two Klingons at same position
            let state = makeStateWithShields [| mkKlingon 4 4 200.0; mkKlingon 4 4 200.0 |] { X = 4; Y = 4 } 1000.0
            let msgs, newState = klingonAttack state
            let singleDamage = 400.0 * 30.0 / 31.0
            Expect.equal msgs.Length 2 "should have two hit messages"
            Expect.floatClose Accuracy.medium newState.Enterprise.Shields (1000.0 - 2.0 * singleDamage) "shields reduced by cumulative damage"

        testCase "hit message format is correct" <| fun _ ->
            let state = makeStateWithShields [| mkKlingon 3 5 200.0 |] { X = 4; Y = 4 } 500.0
            let msgs, _ = klingonAttack state
            Expect.equal msgs.Length 1 "should have one message"
            Expect.isTrue (msgs.[0].Contains("UNIT HIT ON ENTERPRISE FROM SECTOR 3,5")) "should contain hit description"
            Expect.isTrue (msgs.[0].Contains("LEFT)")) "should contain remaining shields"

        testCase "Enterprise destruction when shields insufficient" <| fun _ ->
            // Klingon with 200 energy -> blast 400, damage = 400*30/31 ~= 387
            // Shields = 100, so shields go to ~-287 -> Destroyed
            let state = makeStateWithShields [| mkKlingon 4 4 200.0 |] { X = 4; Y = 4 } 100.0
            let _, newState = klingonAttack state
            Expect.isTrue (newState.Enterprise.Shields < 0.0) "shields should be negative"
            Expect.equal (condition newState.Enterprise) Destroyed "should be destroyed"

        testCase "Klingon at distance deals less damage" <| fun _ ->
            // Enterprise at 1,1, Klingon at 4,4 -> dx=3, dy=3 -> dist^2=18
            // blastEnergy = 200*2 = 400, damage = 400*30/(30+18+1) = 400*30/49
            let state = makeStateWithShields [| mkKlingon 4 4 200.0 |] { X = 1; Y = 1 } 500.0
            let _, newState = klingonAttack state
            let expectedDamage = 400.0 * 30.0 / 49.0
            Expect.floatClose Accuracy.medium newState.Enterprise.Shields (500.0 - expectedDamage) "shields should reflect distance-based damage"
    ]
