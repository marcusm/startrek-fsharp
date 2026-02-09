module StarTrek.Tests.GalaxyTests

open Expecto
open StarTrek.GameTypes
open StarTrek.Galaxy

[<Tests>]
let galaxyTests =
    testList "Galaxy" [
        testCase "encodeQuadrant encodes klingons starbases and stars" <| fun _ ->
            let quadrant = { Quadrant = { X = 0; Y = 0 }; Klingons = 3; Starbases = 1; Stars = 5 }
            Expect.equal (encodeQuadrant quadrant) 315 "should encode as 315"

        testCase "decodeQuadrant reverses encodeQuadrant" <| fun _ ->
            let quadrant = { Quadrant = { X = 0; Y = 0 }; Klingons = 2; Starbases = 0; Stars = 7 }
            let decoded = decodeQuadrant (encodeQuadrant quadrant)
            Expect.equal decoded.Klingons quadrant.Klingons "klingons should match"
            Expect.equal decoded.Starbases quadrant.Starbases "starbases should match"
            Expect.equal decoded.Stars quadrant.Stars "stars should match"

        testCase "encodeQuadrant with zeros returns zero" <| fun _ ->
            let quadrant = { Quadrant = { X = 0; Y = 0 }; Klingons = 0; Starbases = 0; Stars = 0 }
            Expect.equal (encodeQuadrant quadrant) 0 "should encode as 0"

        testCase "isValidPosition accepts positions within bounds" <| fun _ ->
            Expect.isTrue (isValidPosition { X = 1; Y = 1 }) "1,1 is valid"
            Expect.isTrue (isValidPosition { X = 8; Y = 8 }) "8,8 is valid"
            Expect.isTrue (isValidPosition { X = 3; Y = 5 }) "interior is valid"

        testCase "isValidPosition rejects positions out of bounds" <| fun _ ->
            Expect.isFalse (isValidPosition { X = 0; Y = 1 }) "X=0 is invalid"
            Expect.isFalse (isValidPosition { X = 1; Y = 0 }) "Y=0 is invalid"
            Expect.isFalse (isValidPosition { X = 9; Y = 1 }) "X=9 is invalid"
            Expect.isFalse (isValidPosition { X = 1; Y = 9 }) "Y=9 is invalid"

        testCase "sectorToChar returns correct symbols" <| fun _ ->
            Expect.equal (sectorToChar Enterprise) "<*>" "enterprise"
            Expect.equal (sectorToChar (Klingon 200.0)) "+++" "klingon"
            Expect.equal (sectorToChar Starbase) ">!<" "starbase"
            Expect.equal (sectorToChar Star) " * " "star"
            Expect.equal (sectorToChar Empty) "   " "empty"

        testCase "getCourseVector returns None for invalid course" <| fun _ ->
            Expect.isNone (getCourseVector 0.5) "below 1 is invalid"
            Expect.isNone (getCourseVector 9.0) "9.0 is invalid"

        testCase "getCourseVector returns Some for valid course" <| fun _ ->
            Expect.isSome (getCourseVector 1.0) "1.0 is valid"
            Expect.isSome (getCourseVector 4.5) "4.5 is valid"
            Expect.isSome (getCourseVector 8.99) "8.99 is valid"

        testCase "createEmptySectorMap creates 8x8 grid of Empty" <| fun _ ->
            let map = createEmptySectorMap ()
            Expect.equal (Array2D.length1 map) 8 "8 rows"
            Expect.equal (Array2D.length2 map) 8 "8 columns"
            for y in 0..7 do
                for x in 0..7 do
                    Expect.equal map.[y, x] Empty $"cell [{y},{x}] should be Empty"
    ]

[<Tests>]
let initializationTests =
    testList "Initialization" [
        testCase "enterprise positions are within bounds" <| fun _ ->
            let state = initializeGame 42
            Expect.isTrue (isValidPosition state.Enterprise.Quadrant) "quadrant in bounds"
            Expect.isTrue (isValidPosition state.Enterprise.Sector) "sector in bounds"

        testCase "total klingons at least 12" <| fun _ ->
            let state = initializeGame 42
            let mutable total = 0
            state.Quadrants |> Array2D.iter (fun q -> total <- total + q.Klingons)
            Expect.isGreaterThanOrEqual total 12 "should have at least 12 klingons"

        testCase "total starbases at least 2" <| fun _ ->
            let state = initializeGame 42
            let mutable total = 0
            state.Quadrants |> Array2D.iter (fun q -> total <- total + q.Starbases)
            Expect.isGreaterThanOrEqual total 2 "should have at least 2 starbases"

        testCase "enterprise starts with full supplies" <| fun _ ->
            let state = initializeGame 42
            Expect.equal state.Enterprise.Energy 3000.0 "energy"
            Expect.equal state.Enterprise.Shields 0.0 "shields"
            Expect.equal state.Enterprise.Torpedoes 10 "torpedoes"

        testCase "stardate current equals start" <| fun _ ->
            let state = initializeGame 42
            Expect.equal state.Stardate.Current state.Stardate.Start "current should equal start"

        testCase "stardate in range 2000-3999" <| fun _ ->
            let state = initializeGame 42
            Expect.isGreaterThanOrEqual state.Stardate.Start 2000 "start >= 2000"
            Expect.isLessThanOrEqual state.Stardate.Start 3999 "start <= 3999"

        testCase "all quadrants have 1-8 stars" <| fun _ ->
            let state = initializeGame 42
            state.Quadrants
            |> Array2D.iter (fun q ->
                Expect.isGreaterThanOrEqual q.Stars 1 $"quadrant {q.Quadrant} stars >= 1"
                Expect.isLessThanOrEqual q.Stars 8 $"quadrant {q.Quadrant} stars <= 8")

        testCase "all quadrants have 0-3 klingons" <| fun _ ->
            let state = initializeGame 42
            state.Quadrants
            |> Array2D.iter (fun q ->
                Expect.isGreaterThanOrEqual q.Klingons 0 $"quadrant {q.Quadrant} klingons >= 0"
                Expect.isLessThanOrEqual q.Klingons 3 $"quadrant {q.Quadrant} klingons <= 3")

        testCase "quadrant grid is 8x8" <| fun _ ->
            let state = initializeGame 42
            Expect.equal (Array2D.length1 state.Quadrants) 8 "8 rows"
            Expect.equal (Array2D.length2 state.Quadrants) 8 "8 columns"

        testCase "deterministic with same seed" <| fun _ ->
            let state1 = initializeGame 123
            let state2 = initializeGame 123
            Expect.equal state1.Enterprise.Quadrant state2.Enterprise.Quadrant "same quadrant"
            Expect.equal state1.Enterprise.Sector state2.Enterprise.Sector "same sector"
            Expect.equal state1.Stardate state2.Stardate "same stardate"

        testCase "different seeds produce different states" <| fun _ ->
            let state1 = initializeGame 100
            let state2 = initializeGame 200
            let sameQuadrant = state1.Enterprise.Quadrant = state2.Enterprise.Quadrant
            let sameSector = state1.Enterprise.Sector = state2.Enterprise.Sector
            let sameDate = state1.Stardate.Start = state2.Stardate.Start
            Expect.isFalse (sameQuadrant && sameSector && sameDate) "should differ somewhere"

        testCase "quadrant positions are 1-indexed and match array indices" <| fun _ ->
            let state = initializeGame 42
            for x in 0..7 do
                for y in 0..7 do
                    let q = state.Quadrants.[x, y]
                    Expect.equal q.Quadrant.X (x + 1) $"quadrant [{x},{y}] X should be {x + 1}"
                    Expect.equal q.Quadrant.Y (y + 1) $"quadrant [{x},{y}] Y should be {y + 1}"
    ]

[<Tests>]
let enterQuadrantTests =
    testList "enterQuadrant" [
        testCase "places enterprise at its sector position" <| fun _ ->
            let state = initializeGame 42
            let ep = state.Enterprise.Sector
            let cell = state.CurrentQuadrant.[ep.Y - 1, ep.X - 1]
            Expect.equal cell Enterprise "enterprise should be at its sector position"

        testCase "places correct number of klingons" <| fun _ ->
            let state = initializeGame 42
            let qx = state.Enterprise.Quadrant.X - 1
            let qy = state.Enterprise.Quadrant.Y - 1
            let expectedKlingons = state.Quadrants.[qx, qy].Klingons
            let actualKlingons =
                state.CurrentQuadrant
                |> Seq.cast<Sector>
                |> Seq.filter (fun s -> match s with Klingon _ -> true | _ -> false)
                |> Seq.length
            Expect.equal actualKlingons expectedKlingons "klingon count should match quadrant metadata"
            Expect.equal state.Klingons.Length expectedKlingons "Klingons array should match"

        testCase "places correct number of starbases" <| fun _ ->
            let state = initializeGame 42
            let qx = state.Enterprise.Quadrant.X - 1
            let qy = state.Enterprise.Quadrant.Y - 1
            let expectedBases = state.Quadrants.[qx, qy].Starbases
            let actualBases =
                state.CurrentQuadrant
                |> Seq.cast<Sector>
                |> Seq.filter (fun s -> match s with Starbase -> true | _ -> false)
                |> Seq.length
            Expect.equal actualBases expectedBases "starbase count should match quadrant metadata"

        testCase "places correct number of stars" <| fun _ ->
            let state = initializeGame 42
            let qx = state.Enterprise.Quadrant.X - 1
            let qy = state.Enterprise.Quadrant.Y - 1
            let expectedStars = state.Quadrants.[qx, qy].Stars
            let actualStars =
                state.CurrentQuadrant
                |> Seq.cast<Sector>
                |> Seq.filter (fun s -> match s with Star -> true | _ -> false)
                |> Seq.length
            Expect.equal actualStars expectedStars "star count should match quadrant metadata"

        testCase "no collisions - all placed items at distinct positions" <| fun _ ->
            let state = initializeGame 42
            let occupied =
                seq {
                    for row in 0..7 do
                        for col in 0..7 do
                            match state.CurrentQuadrant.[row, col] with
                            | Empty -> ()
                            | _ -> yield (row, col)
                }
                |> Seq.toList
            let distinctCount = occupied |> List.distinct |> List.length
            Expect.equal distinctCount occupied.Length "all occupied positions should be distinct"

        testCase "deterministic with same seed" <| fun _ ->
            let state1 = initializeGame 123
            let state2 = initializeGame 123
            for row in 0..7 do
                for col in 0..7 do
                    Expect.equal state1.CurrentQuadrant.[row, col] state2.CurrentQuadrant.[row, col]
                        $"sector [{row},{col}] should be identical"

        testCase "klingons have 200 energy" <| fun _ ->
            let state = initializeGame 42
            for k in state.Klingons do
                Expect.equal k.Energy 200.0 "klingon energy should be 200"

        testCase "returns docking message when adjacent to starbase" <| fun _ ->
            let state = initializeGame 42
            // Place Enterprise adjacent to a starbase by setting up a quadrant with a starbase
            // and positioning Enterprise so it will dock
            let enterprise = StarTrek.Enterprise.resetEnterprise { X = 4; Y = 4 } { X = 4; Y = 4 }
            let quadrants = Array2D.init 8 8 (fun x y ->
                { Quadrant = { X = x + 1; Y = y + 1 }; Klingons = 0; Starbases = 0; Stars = 0 })
            // Put a starbase in the quadrant
            quadrants.[3, 3] <- { quadrants.[3, 3] with Starbases = 1; Stars = 1 }
            let fixedState =
                { state with
                    Enterprise = enterprise
                    Quadrants = quadrants
                    CurrentQuadrant = Array2D.create 8 8 Empty }
            let msgs, newState = enterQuadrant fixedState
            // With 1 starbase placed randomly, it might or might not land adjacent.
            // Instead, check: if docked, message appears; if not docked, no message.
            if isDocked newState then
                Expect.contains msgs "SHIELDS DROPPED FOR DOCKING PURPOSES" "docking message should be returned"
            else
                Expect.isEmpty msgs "no docking message when not adjacent"

        testCase "returns no messages when not near starbase" <| fun _ ->
            let state = initializeGame 42
            let enterprise = StarTrek.Enterprise.resetEnterprise { X = 4; Y = 4 } { X = 4; Y = 4 }
            let quadrants = Array2D.init 8 8 (fun x y ->
                { Quadrant = { X = x + 1; Y = y + 1 }; Klingons = 0; Starbases = 0; Stars = 1 })
            let fixedState =
                { state with
                    Enterprise = enterprise
                    Quadrants = quadrants
                    CurrentQuadrant = Array2D.create 8 8 Empty }
            let msgs, _ = enterQuadrant fixedState
            Expect.isEmpty msgs "no messages when no starbase in quadrant"
    ]

[<Tests>]
let redAlertOnEntryTests =
    testList "redAlertMessages" [
        testCase "red alert when entering quadrant with Klingons and low shields" <| fun _ ->
            let state = initializeGame 42
            let enterprise = { StarTrek.Enterprise.resetEnterprise { X = 4; Y = 4 } { X = 4; Y = 4 } with Shields = 100.0 }
            let quadrants = Array2D.init 8 8 (fun x y ->
                { Quadrant = { X = x + 1; Y = y + 1 }; Klingons = 0; Starbases = 0; Stars = 1 })
            quadrants.[3, 3] <- { quadrants.[3, 3] with Klingons = 2 }
            let fixedState =
                { state with
                    Enterprise = enterprise
                    Quadrants = quadrants
                    CurrentQuadrant = Array2D.create 8 8 Empty }
            let msgs, _ = enterQuadrant fixedState
            Expect.contains msgs "COMBAT AREA      CONDITION RED" "should show red alert"
            Expect.contains msgs "   SHIELDS DANGEROUSLY LOW" "should warn about low shields"

        testCase "red alert at exactly 200 shields" <| fun _ ->
            let state = initializeGame 42
            let enterprise = { StarTrek.Enterprise.resetEnterprise { X = 4; Y = 4 } { X = 4; Y = 4 } with Shields = 200.0 }
            let quadrants = Array2D.init 8 8 (fun x y ->
                { Quadrant = { X = x + 1; Y = y + 1 }; Klingons = 0; Starbases = 0; Stars = 1 })
            quadrants.[3, 3] <- { quadrants.[3, 3] with Klingons = 1 }
            let fixedState =
                { state with
                    Enterprise = enterprise
                    Quadrants = quadrants
                    CurrentQuadrant = Array2D.create 8 8 Empty }
            let msgs, _ = enterQuadrant fixedState
            Expect.contains msgs "COMBAT AREA      CONDITION RED" "should show red alert at exactly 200"

        testCase "no red alert when shields above 200" <| fun _ ->
            let state = initializeGame 42
            let enterprise = { StarTrek.Enterprise.resetEnterprise { X = 4; Y = 4 } { X = 4; Y = 4 } with Shields = 300.0 }
            let quadrants = Array2D.init 8 8 (fun x y ->
                { Quadrant = { X = x + 1; Y = y + 1 }; Klingons = 0; Starbases = 0; Stars = 1 })
            quadrants.[3, 3] <- { quadrants.[3, 3] with Klingons = 1 }
            let fixedState =
                { state with
                    Enterprise = enterprise
                    Quadrants = quadrants
                    CurrentQuadrant = Array2D.create 8 8 Empty }
            let msgs, _ = enterQuadrant fixedState
            let hasAlert = msgs |> List.exists (fun m -> m = "COMBAT AREA      CONDITION RED")
            Expect.isFalse hasAlert "should not show red alert when shields > 200"

        testCase "no red alert when no Klingons" <| fun _ ->
            let state = initializeGame 42
            let enterprise = { StarTrek.Enterprise.resetEnterprise { X = 4; Y = 4 } { X = 4; Y = 4 } with Shields = 50.0 }
            let quadrants = Array2D.init 8 8 (fun x y ->
                { Quadrant = { X = x + 1; Y = y + 1 }; Klingons = 0; Starbases = 0; Stars = 1 })
            let fixedState =
                { state with
                    Enterprise = enterprise
                    Quadrants = quadrants
                    CurrentQuadrant = Array2D.create 8 8 Empty }
            let msgs, _ = enterQuadrant fixedState
            let hasAlert = msgs |> List.exists (fun m -> m = "COMBAT AREA      CONDITION RED")
            Expect.isFalse hasAlert "should not show red alert without Klingons"
    ]

[<Tests>]
let longRangeScanTests =
    testList "longRangeScan" [
        testCase "header contains enterprise quadrant" <| fun _ ->
            let state = initializeGame 42
            let lines, _ = longRangeScan state
            let qx = state.Enterprise.Quadrant.X
            let qy = state.Enterprise.Quadrant.Y
            Expect.equal lines.[0] (sprintf "LONG RANGE SCAN FOR QUADRANT %d,%d" qx qy) "header should show quadrant"

        testCase "output has correct structure" <| fun _ ->
            let state = initializeGame 42
            let lines, _ = longRangeScan state
            // header + (separator + data row) * 3 + trailing separator = 1 + 3*2 + 1 = 8 but
            // actually: header, sep, row, sep, row, sep, row, sep = 8 lines
            Expect.equal lines.Length 8 "should have 8 lines (header + 3 data rows with separators)"
            Expect.equal lines.[1] "-------------------" "first separator"
            Expect.equal lines.[3] "-------------------" "second separator"
            Expect.equal lines.[5] "-------------------" "third separator"
            Expect.equal lines.[7] "-------------------" "fourth separator"

        testCase "center cell matches enterprise quadrant encoding" <| fun _ ->
            let state = initializeGame 42
            let lines, _ = longRangeScan state
            let qx = state.Enterprise.Quadrant.X - 1
            let qy = state.Enterprise.Quadrant.Y - 1
            let expected = encodeQuadrant state.Quadrants.[qx, qy]
            let centerRow = lines.[4] // middle data row (header, sep, row1, sep, row2)
            Expect.stringContains centerRow (sprintf "%03d" expected) "center should contain enterprise quadrant code"

        testCase "corner quadrant shows out-of-bounds markers" <| fun _ ->
            let state = initializeGame 42
            let cornerState =
                { state with Enterprise = { state.Enterprise with Quadrant = { X = 1; Y = 1 } } }
            let lines, _ = longRangeScan cornerState
            // Top-left corner: row 0 should have *** for all out-of-bounds
            Expect.stringContains lines.[2] "***" "top row should have out-of-bounds marker"
            // First data row (dy=-1): all three cells have Y=0 which is out of bounds
            // The row pattern is ": XXX : XXX : XXX :"
            Expect.equal lines.[2] ": *** : *** : *** :" "entire top row out of bounds at corner 1,1"

        testCase "edge quadrant has partial out-of-bounds" <| fun _ ->
            let state = initializeGame 42
            let edgeState =
                { state with Enterprise = { state.Enterprise with Quadrant = { X = 1; Y = 4 } } }
            let lines, _ = longRangeScan edgeState
            // X=1 means dx=-1 gives X=0 (out of bounds), dx=0 and dx=1 are valid
            // Each data row should start with *** then two valid entries
            for rowIdx in [2; 4; 6] do
                Expect.stringContains lines.[rowIdx] "***" $"row {rowIdx} should have out-of-bounds on left"

        testCase "long range scan updates quadrantsScanned" <| fun _ ->
            let state = initializeGame 42
            let _, scanned = longRangeScan state
            let qx = state.Enterprise.Quadrant.X
            let qy = state.Enterprise.Quadrant.Y
            // The enterprise quadrant itself should be scanned
            Expect.isTrue (Set.contains { X = qx; Y = qy } scanned) "enterprise quadrant should be scanned"

        testCase "isLongRangeScannersDamaged returns false when undamaged" <| fun _ ->
            let state = initializeGame 42
            Expect.isFalse (isLongRangeScannersDamaged state.Enterprise) "should not be damaged initially"

        testCase "isLongRangeScannersDamaged returns true when damaged" <| fun _ ->
            let state = initializeGame 42
            let damagedEnterprise =
                { state.Enterprise with
                    Damage = state.Enterprise.Damage |> List.map (fun d ->
                        if d.System = LongRangeSensors then { d with Amount = -1 } else d) }
            Expect.isTrue (isLongRangeScannersDamaged damagedEnterprise) "should be damaged"
    ]

type FixedRandom(value: float) =
    interface IRandomService with
        member _.Next() = 0
        member _.Next(_max: int) = 0
        member _.Next(min: int, _max: int) = min
        member _.NextDouble() = value

let private mkKlingon x y energy : Klingon =
    { Sector = { X = x; Y = y }; Energy = energy }

let private makeSimpleState () =
    let enterprise = StarTrek.Enterprise.resetEnterprise { X = 4; Y = 4 } { X = 4; Y = 4 }
    let quadrants = Array2D.init 8 8 (fun x y ->
        { Quadrant = { X = x + 1; Y = y + 1 }; Klingons = 0; Starbases = 0; Stars = 4 })
    quadrants.[0, 0] <- { quadrants.[0, 0] with Klingons = 2; Starbases = 1; Stars = 3 }
    quadrants.[3, 3] <- { quadrants.[3, 3] with Klingons = 1; Starbases = 0; Stars = 5 }
    let sectorMap = Array2D.create 8 8 Empty
    sectorMap.[3, 3] <- Sector.Enterprise
    { Enterprise = enterprise
      Klingons = [||]
      CurrentQuadrant = sectorMap
      Quadrants = quadrants
      Stardate = { Current = 2500; Start = 2500; Turns = 30 }
      QuadrantsScanned = Set.empty
      Random = FixedRandom(0.5) :> IRandomService
      InitialKlingons = 3 }

[<Tests>]
let galacticRecordTests =
    testList "galacticRecordLines" [
        testCase "header contains CUMULATIVE GALACTIC RECORD" <| fun _ ->
            let state = makeSimpleState ()
            let lines = galacticRecordLines state
            Expect.isTrue (lines.[0].Contains("CUMULATIVE GALACTIC RECORD")) "should have header"

        testCase "unscanned quadrants show ???" <| fun _ ->
            let state = makeSimpleState ()
            let lines = galacticRecordLines state
            // No quadrants scanned, all should show ???
            let dataLines = lines |> List.filter (fun l -> l.Contains("???"))
            Expect.isNonEmpty dataLines "all unscanned quadrants should show ???"

        testCase "scanned quadrants show 3-digit code" <| fun _ ->
            let state = makeSimpleState ()
            // Mark quadrant 1,1 as scanned
            let state = { state with QuadrantsScanned = Set.singleton { X = 1; Y = 1 } }
            let lines = galacticRecordLines state
            // quadrant[0,0] has K=2, B=1, S=3 -> encoded as 213
            let hasCode = lines |> List.exists (fun l -> l.Contains("213"))
            Expect.isTrue hasCode "scanned quadrant should show encoded value 213"

        testCase "output has separator lines" <| fun _ ->
            let state = makeSimpleState ()
            let lines = galacticRecordLines state
            let separators = lines |> List.filter (fun l -> l.StartsWith("---"))
            Expect.equal separators.Length 9 "should have 9 separator lines"

        testCase "total output has correct line count" <| fun _ ->
            let state = makeSimpleState ()
            let lines = galacticRecordLines state
            // header + [for each of 8 rows: data + separator] + initial separator = 1 + 1 + 16 = 18
            Expect.equal lines.Length 18 "should have 18 lines"

        testCase "data rows use colon-separated format" <| fun _ ->
            let state = makeSimpleState ()
            let lines = galacticRecordLines state
            // Data rows (indices 2, 4, 6, ...) should start with ': ' and end with ' :'
            let dataLines = lines |> List.filter (fun l -> l.StartsWith(":") && l.EndsWith(":"))
            Expect.equal dataLines.Length 8 "should have 8 data rows in colon format"
    ]

[<Tests>]
let statusReportTests =
    testList "statusReportLines" [
        testCase "shows correct klingon count" <| fun _ ->
            let state = makeSimpleState ()
            // Total klingons: quadrant[0,0]=2, quadrant[3,3]=1 = 3
            let lines = statusReportLines state
            Expect.isTrue (lines |> List.exists (fun l -> l.Contains("KLINGON SHIPS LEFT : 3"))) "should show 3 klingons"

        testCase "shows correct stardates left" <| fun _ ->
            let state = makeSimpleState ()
            // Turns=30, Current=2500, Start=2500 -> left = 30 - (2500-2500) = 30
            let lines = statusReportLines state
            Expect.isTrue (lines |> List.exists (fun l -> l.Contains("STARDATES LEFT     : 30"))) "should show 30 stardates"

        testCase "shows correct stardates after time passes" <| fun _ ->
            let state = makeSimpleState ()
            let state = { state with Stardate = { state.Stardate with Current = 2510 } }
            // left = 30 - (2510-2500) = 20
            let lines = statusReportLines state
            Expect.isTrue (lines |> List.exists (fun l -> l.Contains("STARDATES LEFT     : 20"))) "should show 20 stardates"

        testCase "shows correct starbase count" <| fun _ ->
            let state = makeSimpleState ()
            // quadrant[0,0] has 1 starbase
            let lines = statusReportLines state
            Expect.isTrue (lines |> List.exists (fun l -> l.Contains("STARBASES LEFT     : 1"))) "should show 1 starbase"

        testCase "header contains STATUS REPORT" <| fun _ ->
            let state = makeSimpleState ()
            let lines = statusReportLines state
            Expect.isTrue (lines.[0].Contains("STATUS REPORT")) "should have status report header"
    ]

[<Tests>]
let torpedoDataEdgeCaseTests =
    testList "torpedoDataLines edge cases" [
        testCase "no klingons shows sensor report" <| fun _ ->
            let state = makeSimpleState ()
            let lines = torpedoDataLines state
            Expect.isTrue (lines |> List.exists (fun l -> l.Contains("NO ENEMY SHIPS"))) "should report no enemies"

        testCase "single klingon shows distance and course" <| fun _ ->
            let enterprise = StarTrek.Enterprise.resetEnterprise { X = 4; Y = 4 } { X = 4; Y = 4 }
            let klingons = [| mkKlingon 4 2 200.0 |]
            let sectorMap = Array2D.create 8 8 Empty
            sectorMap.[3, 3] <- Sector.Enterprise
            sectorMap.[1, 3] <- Klingon 200.0
            let quadrants = Array2D.init 8 8 (fun x y ->
                { Quadrant = { X = x + 1; Y = y + 1 }; Klingons = 0; Starbases = 0; Stars = 4 })
            let state =
                { Enterprise = enterprise; Klingons = klingons; CurrentQuadrant = sectorMap
                  Quadrants = quadrants; Stardate = { Current = 2500; Start = 2500; Turns = 30 }
                  QuadrantsScanned = Set.empty; Random = FixedRandom(0.5) :> IRandomService; InitialKlingons = 1 }
            let lines = torpedoDataLines state
            Expect.isTrue (lines.[0].Contains("PHOTON TORPEDO DATA")) "should have torpedo header"
            Expect.isTrue (lines.[1].Contains("KLINGON AT SECTOR 4,2")) "should show klingon coordinates"
            Expect.isTrue (lines.[1].Contains("COURSE")) "should include course"
            Expect.isTrue (lines.[1].Contains("DISTANCE")) "should include distance"

        testCase "multiple klingons get individual data lines" <| fun _ ->
            let enterprise = StarTrek.Enterprise.resetEnterprise { X = 4; Y = 4 } { X = 4; Y = 4 }
            let klingons = [| mkKlingon 4 2 200.0; mkKlingon 6 4 200.0 |]
            let sectorMap = Array2D.create 8 8 Empty
            sectorMap.[3, 3] <- Sector.Enterprise
            sectorMap.[1, 3] <- Klingon 200.0
            sectorMap.[3, 5] <- Klingon 200.0
            let quadrants = Array2D.init 8 8 (fun x y ->
                { Quadrant = { X = x + 1; Y = y + 1 }; Klingons = 0; Starbases = 0; Stars = 4 })
            let state =
                { Enterprise = enterprise; Klingons = klingons; CurrentQuadrant = sectorMap
                  Quadrants = quadrants; Stardate = { Current = 2500; Start = 2500; Turns = 30 }
                  QuadrantsScanned = Set.empty; Random = FixedRandom(0.5) :> IRandomService; InitialKlingons = 2 }
            let lines = torpedoDataLines state
            let klingonLines = lines |> List.filter (fun l -> l.Contains("KLINGON AT SECTOR"))
            Expect.equal klingonLines.Length 2 "should have data for each klingon"

        testCase "due north klingon gives course 3.0" <| fun _ ->
            let enterprise = StarTrek.Enterprise.resetEnterprise { X = 4; Y = 4 } { X = 4; Y = 4 }
            let klingons = [| mkKlingon 4 1 200.0 |]
            let sectorMap = Array2D.create 8 8 Empty
            sectorMap.[3, 3] <- Sector.Enterprise
            sectorMap.[0, 3] <- Klingon 200.0
            let quadrants = Array2D.init 8 8 (fun x y ->
                { Quadrant = { X = x + 1; Y = y + 1 }; Klingons = 0; Starbases = 0; Stars = 4 })
            let state =
                { Enterprise = enterprise; Klingons = klingons; CurrentQuadrant = sectorMap
                  Quadrants = quadrants; Stardate = { Current = 2500; Start = 2500; Turns = 30 }
                  QuadrantsScanned = Set.empty; Random = FixedRandom(0.5) :> IRandomService; InitialKlingons = 1 }
            let lines = torpedoDataLines state
            // North means dy<0, dx=0. atan2(-3, 0) = -PI/2. Course = 1 - (-PI/2)*4/PI = 1+2 = 3.0
            Expect.isTrue (lines.[1].Contains("COURSE = 3.0")) "due north should be course 3.0"

        testCase "due east klingon gives course 1.0" <| fun _ ->
            let enterprise = StarTrek.Enterprise.resetEnterprise { X = 4; Y = 4 } { X = 4; Y = 4 }
            let klingons = [| mkKlingon 7 4 200.0 |]
            let sectorMap = Array2D.create 8 8 Empty
            sectorMap.[3, 3] <- Sector.Enterprise
            sectorMap.[3, 6] <- Klingon 200.0
            let quadrants = Array2D.init 8 8 (fun x y ->
                { Quadrant = { X = x + 1; Y = y + 1 }; Klingons = 0; Starbases = 0; Stars = 4 })
            let state =
                { Enterprise = enterprise; Klingons = klingons; CurrentQuadrant = sectorMap
                  Quadrants = quadrants; Stardate = { Current = 2500; Start = 2500; Turns = 30 }
                  QuadrantsScanned = Set.empty; Random = FixedRandom(0.5) :> IRandomService; InitialKlingons = 1 }
            let lines = torpedoDataLines state
            // East means dy=0, dx>0. atan2(0, 3) = 0. Course = 1 - 0 = 1.0
            Expect.isTrue (lines.[1].Contains("COURSE = 1.0")) "due east should be course 1.0"
    ]
