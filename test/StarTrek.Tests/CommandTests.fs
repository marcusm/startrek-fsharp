module StarTrek.Tests.CommandTests

open Expecto
open StarTrek.GameTypes
open StarTrek.Enterprise
open StarTrek.Galaxy
open StarTrek.App.Commands

type FixedRandom(value: float) =
    interface IRandomService with
        member _.Next() = 0
        member _.Next(_max: int) = 0
        member _.Next(min: int, _max: int) = min
        member _.NextDouble() = value

let private mkKlingon x y energy : Klingon =
    { Sector = { X = x; Y = y }; Energy = energy }

/// Create a minimal state with given enterprise sector, optional starbase, klingons
let private makeState (enterpriseSector: Position) (starbasePos: Position option) (klingons: Klingon array) =
    let enterprise = resetEnterprise { X = 4; Y = 4 } enterpriseSector
    let sectorMap = Array2D.create 8 8 Empty
    sectorMap.[enterpriseSector.Y - 1, enterpriseSector.X - 1] <- Sector.Enterprise
    starbasePos |> Option.iter (fun pos ->
        sectorMap.[pos.Y - 1, pos.X - 1] <- Starbase)
    for k in klingons do
        sectorMap.[k.Sector.Y - 1, k.Sector.X - 1] <- Klingon k.Energy
    let quadrants = Array2D.init 8 8 (fun x y ->
        { Quadrant = { X = x + 1; Y = y + 1 }; Klingons = 0; Starbases = 0; Stars = 4 })
    let qx = enterprise.Quadrant.X - 1
    let qy = enterprise.Quadrant.Y - 1
    let starbases = if starbasePos.IsSome then 1 else 0
    quadrants.[qx, qy] <- { quadrants.[qx, qy] with Klingons = Array.length klingons; Starbases = starbases }
    { Enterprise = enterprise
      Klingons = klingons
      CurrentQuadrant = sectorMap
      Quadrants = quadrants
      Stardate = { Current = 2500; Start = 2500; Turns = 30 }
      QuadrantsScanned = Set.empty
      Random = FixedRandom(0.5) :> IRandomService
      InitialKlingons = Array.length klingons }

let private damageSystem (sys: SystemDamage) (amount: int) (state: GameState) =
    let newDamage =
        state.Enterprise.Damage
        |> List.map (fun d -> if d.System = sys then { d with Amount = amount } else d)
    { state with Enterprise = { state.Enterprise with Damage = newDamage } }

// ─── getCondition ───────────────────────────────────────────

[<Tests>]
let getConditionTests =
    testList "getCondition" [
        testCase "GREEN when no klingons, energy >= 300, not docked" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            Expect.equal (getCondition state) "GREEN" "should be GREEN"

        testCase "YELLOW when no klingons and energy < 300" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let state = { state with Enterprise = { state.Enterprise with Energy = 299.0 } }
            Expect.equal (getCondition state) "YELLOW" "should be YELLOW when energy < 300"

        testCase "YELLOW at exactly 299 energy" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let state = { state with Enterprise = { state.Enterprise with Energy = 299.9 } }
            Expect.equal (getCondition state) "YELLOW" "should be YELLOW at 299.9"

        testCase "GREEN at exactly 300 energy" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let state = { state with Enterprise = { state.Enterprise with Energy = 300.0 } }
            Expect.equal (getCondition state) "GREEN" "should be GREEN at exactly 300"

        testCase "RED when klingons present" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [| mkKlingon 6 6 200.0 |]
            Expect.equal (getCondition state) "RED" "should be RED with klingons"

        testCase "RED overrides YELLOW when klingons present and low energy" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [| mkKlingon 6 6 200.0 |]
            let state = { state with Enterprise = { state.Enterprise with Energy = 100.0 } }
            Expect.equal (getCondition state) "RED" "RED should take priority over YELLOW"

        testCase "DOCKED when adjacent to starbase" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } (Some { X = 4; Y = 5 }) [||]
            Expect.equal (getCondition state) "DOCKED" "should be DOCKED when adjacent to starbase"

        testCase "DOCKED overrides RED when adjacent to starbase with klingons" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } (Some { X = 4; Y = 5 }) [| mkKlingon 6 6 200.0 |]
            Expect.equal (getCondition state) "DOCKED" "DOCKED should override RED"
    ]

// ─── warpStart ──────────────────────────────────────────────

[<Tests>]
let warpStartTests =
    testList "warpStart" [
        testCase "returns empty list when engines undamaged" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs = warpStart state
            Expect.isEmpty msgs "no warning when undamaged"

        testCase "returns warning with max warp when engines damaged" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||] |> damageSystem WarpEngines -3
            let msgs = warpStart state
            Expect.isNonEmpty msgs "should warn when damaged"
            Expect.isTrue (msgs.[0].Contains("DAMAGED")) "should mention damaged"
            Expect.isTrue (msgs.[0].Contains("0.2")) "should show max warp 0.2"
    ]

// ─── warpValidateCourse ─────────────────────────────────────

[<Tests>]
let warpValidateCourseTests =
    testList "warpValidateCourse" [
        testCase "valid course 1.0 returns Ok" <| fun _ ->
            Expect.isOk (warpValidateCourse "1.0") "1.0 should be valid"

        testCase "valid course 5.5 returns Ok" <| fun _ ->
            Expect.isOk (warpValidateCourse "5.5") "5.5 should be valid"

        testCase "valid course 8.99 returns Ok" <| fun _ ->
            Expect.isOk (warpValidateCourse "8.99") "8.99 should be valid"

        testCase "course 0.5 returns error" <| fun _ ->
            Expect.isError (warpValidateCourse "0.5") "0.5 should be invalid"

        testCase "course 9.0 returns error" <| fun _ ->
            Expect.isError (warpValidateCourse "9.0") "9.0 should be invalid"

        testCase "non-numeric returns error" <| fun _ ->
            Expect.isError (warpValidateCourse "abc") "non-numeric should be invalid"
    ]

// ─── warpValidateAndExecute ─────────────────────────────────

[<Tests>]
let warpValidateAndExecuteTests =
    testList "warpValidateAndExecute" [
        testCase "warp factor 0 is rejected" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, newState = warpValidateAndExecute 1.0 "0" state
            Expect.isNonEmpty msgs "should reject warp factor 0"
            Expect.isTrue (msgs.[0].Contains("ENGINES WON'T TAKE")) "should mention engines"
            Expect.equal newState.Enterprise.Sector state.Enterprise.Sector "position unchanged"

        testCase "negative warp factor is rejected" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, _ = warpValidateAndExecute 1.0 "-1" state
            Expect.isNonEmpty msgs "should reject negative warp"

        testCase "warp factor exceeding max is rejected" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, _ = warpValidateAndExecute 1.0 "9" state
            Expect.isNonEmpty msgs "should reject warp > max"
            Expect.isTrue (msgs.[0].Contains("ENGINES WON'T TAKE WARP 9")) "should show the bad warp factor"

        testCase "non-numeric warp factor is rejected" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, _ = warpValidateAndExecute 1.0 "xyz" state
            Expect.isNonEmpty msgs "should reject non-numeric"
    ]

// ─── phaserStart ────────────────────────────────────────────

[<Tests>]
let phaserStartTests =
    testList "phaserStart" [
        testCase "returns disabled message when phasers damaged" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [| mkKlingon 6 6 200.0 |] |> damageSystem Phasers -2
            let msgs, canFire = phaserStart state
            Expect.isFalse canFire "should not be able to fire"
            Expect.contains msgs "PHASER CONTROL IS DISABLED" "should say disabled"

        testCase "returns no klingons message when none present" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, canFire = phaserStart state
            Expect.isFalse canFire "should not be able to fire"
            Expect.contains msgs "SHORT RANGE SENSORS REPORT NO KLINGONS IN THIS QUADRANT" "should report no klingons"

        testCase "returns energy available when klingons present" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [| mkKlingon 6 6 200.0 |]
            let msgs, canFire = phaserStart state
            Expect.isTrue canFire "should be able to fire"
            Expect.isTrue (msgs.[0].Contains("PHASERS LOCKED ON TARGET")) "should say locked on"
            Expect.isTrue (msgs.[0].Contains("3000")) "should show energy"
    ]

// ─── phaserValidateAndExecute ───────────────────────────────

[<Tests>]
let phaserValidateAndExecuteTests =
    testList "phaserValidateAndExecute" [
        testCase "valid energy fires phasers" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [| mkKlingon 4 5 200.0 |]
            let msgs, newState = phaserValidateAndExecute "500" state
            Expect.isNonEmpty msgs "should have hit messages"
            Expect.equal newState.Enterprise.Energy 2500.0 "energy should decrease"

        testCase "exceeding energy returns error" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [| mkKlingon 4 5 200.0 |]
            let msgs, newState = phaserValidateAndExecute "5000" state
            Expect.isTrue (msgs.[0].Contains("WE HAVE ONLY")) "should mention insufficient energy"
            Expect.equal newState.Enterprise.Energy state.Enterprise.Energy "energy unchanged"

        testCase "non-numeric input returns empty messages" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [| mkKlingon 4 5 200.0 |]
            let msgs, _ = phaserValidateAndExecute "abc" state
            Expect.isEmpty msgs "no messages for invalid input"
    ]

// ─── torpedoStart ───────────────────────────────────────────

[<Tests>]
let torpedoStartTests =
    testList "torpedoStart" [
        testCase "returns not operational when tubes damaged" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||] |> damageSystem PhotonTubes -1
            let msgs, canFire = torpedoStart state
            Expect.isFalse canFire "should not fire"
            Expect.contains msgs "PHOTON TUBES ARE NOT OPERATIONAL" "should say not operational"

        testCase "returns expended when no torpedoes" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let state = { state with Enterprise = { state.Enterprise with Torpedoes = 0 } }
            let msgs, canFire = torpedoStart state
            Expect.isFalse canFire "should not fire"
            Expect.contains msgs "ALL PHOTON TORPEDOES EXPENDED" "should say expended"

        testCase "returns course prompt when ready" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, canFire = torpedoStart state
            Expect.isTrue canFire "should be ready to fire"
            Expect.isNonEmpty msgs "should have prompt"
            Expect.isTrue (msgs.[0].Contains("TORPEDO COURSE")) "should prompt for course"
            Expect.isTrue (msgs.[0].Contains("10")) "should show torpedo count"
    ]

// ─── torpedoValidateAndExecute ──────────────────────────────

[<Tests>]
let torpedoValidateAndExecuteTests =
    testList "torpedoValidateAndExecute" [
        testCase "invalid course returns error" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, _ = torpedoValidateAndExecute "0" state
            Expect.isTrue (msgs.[0].Contains("INCORRECT COURSE DATA")) "should report bad course"

        testCase "course 9.0 returns error" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, _ = torpedoValidateAndExecute "9.0" state
            Expect.isTrue (msgs.[0].Contains("INCORRECT COURSE DATA")) "9.0 is invalid"

        testCase "non-numeric returns error" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, _ = torpedoValidateAndExecute "abc" state
            Expect.isTrue (msgs.[0].Contains("INCORRECT COURSE DATA")) "non-numeric invalid"
    ]

// ─── shieldControl ──────────────────────────────────────────

[<Tests>]
let shieldControlTests =
    testList "shieldControl" [
        testCase "returns non-operational when damaged" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||] |> damageSystem ShieldControl -1
            let msgs, _ = shieldControl state
            Expect.contains msgs "SHIELD CONTROL IS NON-OPERATIONAL" "should say non-operational"

        testCase "returns energy prompt when operational" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, _ = shieldControl state
            Expect.isTrue (msgs.[0].Contains("ENERGY AVAILABLE")) "should show energy available"
            Expect.isTrue (msgs.[0].Contains("3000")) "should show total energy"
    ]

// ─── shieldValidateAndExecute ───────────────────────────────

[<Tests>]
let shieldValidateAndExecuteTests =
    testList "shieldValidateAndExecute" [
        testCase "valid input sets shields" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, newState = shieldValidateAndExecute "500" state
            Expect.equal newState.Enterprise.Shields 500.0 "shields should be 500"
            Expect.equal newState.Enterprise.Energy 2500.0 "energy should be 2500"
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("SHIELDS NOW AT 500"))) "should confirm shields"

        testCase "exceeding treasury returns error" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, newState = shieldValidateAndExecute "5000" state
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("TREASURY"))) "should mention treasury"
            Expect.equal newState.Enterprise.Shields state.Enterprise.Shields "shields unchanged"

        testCase "non-numeric returns invalid" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, _ = shieldValidateAndExecute "abc" state
            Expect.contains msgs "INVALID. SHIELDS UNCHANGED" "should say invalid"

        testCase "zero input is rejected per spec" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let state = { state with Enterprise = { state.Enterprise with Energy = 2500.0; Shields = 500.0 } }
            let msgs, newState = shieldValidateAndExecute "0" state
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("INVALID"))) "should say invalid"
            Expect.equal newState.Enterprise.Shields 500.0 "shields unchanged"
    ]

// ─── damageControlReport ────────────────────────────────────

[<Tests>]
let damageControlReportTests =
    testList "damageControlReport" [
        testCase "returns not available when damage control damaged" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||] |> damageSystem DamageControl -1
            let msgs, _ = damageControlReport state
            Expect.contains msgs "DAMAGE CONTROL REPORT IS NOT AVAILABLE" "should say not available"

        testCase "lists all 8 systems when undamaged" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, _ = damageControlReport state
            Expect.isTrue (msgs.[0].Contains("DAMAGE CONTROL REPORT")) "should have header"
            // Header + 8 system lines
            Expect.equal msgs.Length 9 "header + 8 systems"

        testCase "shows damage amounts for damaged systems" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||] |> damageSystem Phasers -3
            let msgs, _ = damageControlReport state
            let phaserLine = msgs |> List.find (fun m -> m.Contains("PHASER"))
            Expect.isTrue (phaserLine.Contains("-3")) "should show -3 damage"
    ]

// ─── libraryComputer ────────────────────────────────────────

[<Tests>]
let libraryComputerTests =
    testList "libraryComputer" [
        testCase "returns disabled when computer damaged" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||] |> damageSystem Computer -1
            let msgs, _ = libraryComputer state
            Expect.contains msgs "COMPUTER DISABLED" "should say disabled"

        testCase "returns menu when operational" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, _ = libraryComputer state
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("COMPUTER ACTIVE"))) "should show active"
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("GALACTIC RECORD"))) "should list option 0"
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("STATUS REPORT"))) "should list option 1"
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("TORPEDO DATA"))) "should list option 2"
    ]

// ─── libraryComputerOption ──────────────────────────────────

[<Tests>]
let libraryComputerOptionTests =
    testList "libraryComputerOption" [
        testCase "option 0 returns galactic record" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, _ = libraryComputerOption "0" state
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("CUMULATIVE GALACTIC RECORD"))) "should have galactic record header"

        testCase "option 1 returns status report" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, _ = libraryComputerOption "1" state
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("STATUS REPORT"))) "should have status report"

        testCase "option 2 returns torpedo data" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [| mkKlingon 6 6 200.0 |]
            let msgs, _ = libraryComputerOption "2" state
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("PHOTON TORPEDO DATA"))) "should have torpedo data"

        testCase "invalid option returns error" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, _ = libraryComputerOption "9" state
            Expect.contains msgs "INVALID COMPUTER OPTION" "should say invalid"

        testCase "option with whitespace is trimmed" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, _ = libraryComputerOption " 1 " state
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("STATUS REPORT"))) "should handle whitespace"
    ]

// ─── shortRangeCommand ──────────────────────────────────────

[<Tests>]
let shortRangeCommandTests =
    testList "shortRangeCommand" [
        testCase "returns out message when SRS damaged" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||] |> damageSystem ShortRangeSensors -1
            let msgs, _ = shortRangeCommand state
            Expect.contains msgs "*** SHORT RANGE SENSORS ARE OUT ***" "should say sensors out"

        testCase "returns scan header with quadrant when operational" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, _ = shortRangeCommand state
            Expect.isTrue (msgs.[0].Contains("SHORT RANGE SENSOR SCAN")) "should have scan header"
            Expect.isTrue (msgs.[0].Contains("4,4")) "should show quadrant"

        testCase "includes condition in output" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, _ = shortRangeCommand state
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("CONDITION: GREEN"))) "should show condition"

        testCase "shows RED condition when klingons present" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [| mkKlingon 6 6 200.0 |]
            let msgs, _ = shortRangeCommand state
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("CONDITION: RED"))) "should show RED"
    ]

// ─── longRangeScan (Commands version) ───────────────────────

[<Tests>]
let longRangeScanCommandTests =
    testList "longRangeScan command" [
        testCase "returns inoperable when LRS damaged" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||] |> damageSystem LongRangeSensors -1
            let msgs, _ = StarTrek.App.Commands.longRangeScan state
            Expect.contains msgs "LONG RANGE SENSORS ARE INOPERABLE" "should say inoperable"

        testCase "returns scan lines when operational" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            let msgs, newState = StarTrek.App.Commands.longRangeScan state
            Expect.isNonEmpty msgs "should have scan output"
            Expect.isTrue (msgs.[0].Contains("LONG RANGE SCAN")) "should have header"
            Expect.isNonEmpty (Set.toList newState.QuadrantsScanned) "should update scanned set"

        testCase "updates QuadrantsScanned set" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            Expect.isEmpty (Set.toList state.QuadrantsScanned) "initially empty"
            let _, newState = StarTrek.App.Commands.longRangeScan state
            Expect.isTrue (Set.contains { X = 4; Y = 4 } newState.QuadrantsScanned) "enterprise quadrant should be scanned"
    ]

// ─── LRS ↔ shield-control independence (§19) ───────────────

[<Tests>]
let lrsShieldIndependenceTests =
    testList "LRS and shield-control independence" [
        testCase "damaged LRS does not affect shield control" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||] |> damageSystem LongRangeSensors -3
            let msgs, _ = shieldControl state
            // Should NOT say non-operational — LRS damage is separate from shield control
            Expect.isFalse (msgs |> List.exists (fun m -> m.Contains("NON-OPERATIONAL"))) "LRS damage should not gate shields"
            Expect.isTrue (msgs.[0].Contains("ENERGY AVAILABLE")) "shield control should work normally"

        testCase "damaged shield control does not affect LRS" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||] |> damageSystem ShieldControl -3
            let msgs, _ = StarTrek.App.Commands.longRangeScan state
            // Should NOT say inoperable — shield control damage is separate from LRS
            Expect.isFalse (msgs |> List.exists (fun m -> m.Contains("INOPERABLE"))) "shield damage should not gate LRS"
            Expect.isTrue (msgs.[0].Contains("LONG RANGE SCAN")) "LRS should work normally"

        testCase "damaging LRS still allows shield transfer" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||] |> damageSystem LongRangeSensors -2
            let msgs, newState = shieldValidateAndExecute "500" state
            Expect.equal newState.Enterprise.Shields 500.0 "shields should transfer despite LRS damage"

        testCase "damaging shield control still allows LRS scan" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||] |> damageSystem ShieldControl -2
            let msgs, newState = StarTrek.App.Commands.longRangeScan state
            Expect.isNonEmpty msgs "should have scan output"
            Expect.isNonEmpty (Set.toList newState.QuadrantsScanned) "should update scanned set"
    ]
