module StarTrek.Tests.WarpTests

open Expecto
open StarTrek.GameTypes
open StarTrek.Galaxy

let private makeTestState () =
    initializeGame 42

let private makeStateWithEnterpriseAt (sx: int) (sy: int) =
    let state = makeTestState ()
    let sectorMap = Array2D.copy state.CurrentQuadrant
    let ep = state.Enterprise.Sector
    sectorMap.[ep.Y - 1, ep.X - 1] <- Empty
    sectorMap.[sy - 1, sx - 1] <- Enterprise
    { state with
        Enterprise = { state.Enterprise with Sector = { X = sx; Y = sy } }
        CurrentQuadrant = sectorMap }

let private clearSectorMap (state: GameState) =
    let sectorMap = Array2D.create galaxySize galaxySize Empty
    let ep = state.Enterprise.Sector
    sectorMap.[ep.Y - 1, ep.X - 1] <- Enterprise
    { state with CurrentQuadrant = sectorMap }

[<Tests>]
let warpCostTests =
    testList "warpEnergyCost" [
        testCase "warp 1 costs 3" <| fun _ ->
            Expect.equal (warpEnergyCost 1.0) 3 "warp 1 = floor(1*8)-5 = 3"

        testCase "warp 0.125 costs -4" <| fun _ ->
            Expect.equal (warpEnergyCost 0.125) -4 "warp 0.125 = floor(0.125*8)-5 = -4"

        testCase "warp 8 costs 59" <| fun _ ->
            Expect.equal (warpEnergyCost 8.0) 59 "warp 8 = floor(8*8)-5 = 59"
    ]

[<Tests>]
let warpDamageTests =
    testList "warp damage" [
        testCase "isWarpDamaged false when healthy" <| fun _ ->
            let state = makeTestState ()
            Expect.isFalse (isWarpDamaged state.Enterprise) "healthy engines not damaged"

        testCase "isWarpDamaged true when damaged" <| fun _ ->
            let state = makeTestState ()
            let damagedEnterprise =
                { state.Enterprise with
                    Damage = state.Enterprise.Damage |> List.map (fun d ->
                        if d.System = WarpEngines then { d with Amount = -3 } else d) }
            Expect.isTrue (isWarpDamaged damagedEnterprise) "damaged engines are damaged"

        testCase "maxWarpFactor is 8.0 when healthy" <| fun _ ->
            let state = makeTestState ()
            Expect.equal (maxWarpFactor state.Enterprise) 8.0 "healthy max is 8.0"

        testCase "maxWarpFactor is 0.2 when damaged" <| fun _ ->
            let state = makeTestState ()
            let damagedEnterprise =
                { state.Enterprise with
                    Damage = state.Enterprise.Damage |> List.map (fun d ->
                        if d.System = WarpEngines then { d with Amount = -1 } else d) }
            Expect.equal (maxWarpFactor damagedEnterprise) 0.2 "damaged max is 0.2"
    ]

[<Tests>]
let executeWarpTests =
    testList "executeWarp" [
        testCase "same quadrant moves Enterprise and deducts energy" <| fun _ ->
            let state = makeStateWithEnterpriseAt 1 4 |> clearSectorMap
            let direction = getCourseVector 1.0 |> Option.get  // East
            let msgs, newState = executeWarp direction 0.5 state  // 4 steps
            Expect.notEqual newState.Enterprise.Sector state.Enterprise.Sector "should have moved"
            Expect.isGreaterThan newState.Enterprise.Sector.X state.Enterprise.Sector.X "should move right"
            Expect.isLessThan newState.Enterprise.Energy state.Enterprise.Energy "energy should decrease"
            Expect.isEmpty msgs "no error messages"

        testCase "blocked by star stops at previous sector" <| fun _ ->
            let state = makeStateWithEnterpriseAt 1 4 |> clearSectorMap
            // Place a star 3 sectors to the right
            let sectorMap = Array2D.copy state.CurrentQuadrant
            sectorMap.[4 - 1, 4 - 1] <- Star  // at (4,4)
            let state = { state with CurrentQuadrant = sectorMap }
            let direction = getCourseVector 1.0 |> Option.get  // East
            let msgs, newState = executeWarp direction 1.0 state
            // Should stop before the star
            Expect.isLessThan newState.Enterprise.Sector.X 4 "should stop before star"
            Expect.isNonEmpty msgs "should have navigation message"

        testCase "insufficient energy refuses and leaves state unchanged" <| fun _ ->
            let state = makeTestState ()
            let lowEnergy = { state with Enterprise = { state.Enterprise with Energy = 5.0 } }
            let direction = getCourseVector 1.0 |> Option.get
            let msgs, newState = executeWarp direction 1.0 lowEnergy
            Expect.equal newState.Enterprise.Sector lowEnergy.Enterprise.Sector "sector unchanged"
            Expect.equal newState.Enterprise.Energy lowEnergy.Enterprise.Energy "energy unchanged"
            Expect.isNonEmpty msgs "should have error message"

        testCase "stardate advances by 1 after successful warp" <| fun _ ->
            let state = makeStateWithEnterpriseAt 4 4 |> clearSectorMap
            let direction = getCourseVector 1.0 |> Option.get
            let _, newState = executeWarp direction 0.125 state
            Expect.equal newState.Stardate.Current (state.Stardate.Current + 1) "stardate +1"

        testCase "crossing quadrant boundary changes quadrant" <| fun _ ->
            let state = makeStateWithEnterpriseAt 8 4 |> clearSectorMap
            // Place Enterprise at quadrant interior so there's room to exit
            let state =
                { state with Enterprise = { state.Enterprise with Quadrant = { X = 4; Y = 4 } } }
            let direction = getCourseVector 1.0 |> Option.get  // East
            let _, newState = executeWarp direction 1.0 state
            Expect.notEqual newState.Enterprise.Quadrant state.Enterprise.Quadrant "quadrant should change"

        testCase "warp past galaxy edge clamps quadrant" <| fun _ ->
            let state = makeStateWithEnterpriseAt 8 4 |> clearSectorMap
            let state =
                { state with Enterprise = { state.Enterprise with Quadrant = { X = 8; Y = 4 } } }
            let direction = getCourseVector 1.0 |> Option.get  // East, already at edge
            let msgs, newState = executeWarp direction 2.0 state
            Expect.equal newState.Enterprise.Quadrant.X 8 "X should be clamped to 8"
            Expect.isNonEmpty msgs "should have perimeter denial message"
    ]

[<Tests>]
let courseDirectionTests =
    testList "course directions" [
        testCase "course 1 moves right (dx > 0)" <| fun _ ->
            let (dx, _dy) = getCourseVector 1.0 |> Option.get
            Expect.isGreaterThan dx 0.0 "course 1 dx should be positive (right)"

        testCase "course 3 moves up (dy < 0)" <| fun _ ->
            let (_dx, dy) = getCourseVector 3.0 |> Option.get
            Expect.isLessThan dy 0.0 "course 3 dy should be negative (up)"

        testCase "course 5 moves left (dx < 0)" <| fun _ ->
            let (dx, _dy) = getCourseVector 5.0 |> Option.get
            Expect.isLessThan dx 0.0 "course 5 dx should be negative (left)"

        testCase "course 7 moves down (dy > 0)" <| fun _ ->
            let (_dx, dy) = getCourseVector 7.0 |> Option.get
            Expect.isGreaterThan dy 0.0 "course 7 dy should be positive (down)"
    ]
