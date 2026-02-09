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

        testCase "all quadrants have 4-6 stars" <| fun _ ->
            let state = initializeGame 42
            state.Quadrants
            |> Array2D.iter (fun q ->
                Expect.isGreaterThanOrEqual q.Stars 4 $"quadrant {q.Quadrant} stars >= 4"
                Expect.isLessThanOrEqual q.Stars 6 $"quadrant {q.Quadrant} stars <= 6")

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
