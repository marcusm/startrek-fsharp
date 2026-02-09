module StarTrek.Tests.DockingTests

open Expecto
open StarTrek.GameTypes
open StarTrek.Enterprise
open StarTrek.Galaxy

type FixedRandom(value: float) =
    interface IRandomService with
        member _.Next() = 0
        member _.Next(_max: int) = 0
        member _.Next(min: int, _max: int) = min
        member _.NextDouble() = value

let private mkKlingon x y energy : Klingon =
    { Sector = { X = x; Y = y }; Energy = energy }

/// Create a state with Enterprise at given sector, optional starbase and klingons
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
      Random = FixedRandom(1.0) :> IRandomService
      InitialKlingons = Array.length klingons }

[<Tests>]
let isDockedTests =
    testList "isDocked" [
        testCase "returns true when adjacent to starbase" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } (Some { X = 4; Y = 5 }) [||]
            Expect.isTrue (isDocked state) "should be docked when adjacent"

        testCase "returns true when diagonally adjacent to starbase" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } (Some { X = 5; Y = 5 }) [||]
            Expect.isTrue (isDocked state) "should be docked when diagonally adjacent"

        testCase "returns false when not adjacent to starbase" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } (Some { X = 4; Y = 7 }) [||]
            Expect.isFalse (isDocked state) "should not be docked when far away"

        testCase "returns false when no starbase in quadrant" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } None [||]
            Expect.isFalse (isDocked state) "should not be docked without starbase"

        testCase "returns true at edge of map adjacent to starbase" <| fun _ ->
            let state = makeState { X = 1; Y = 1 } (Some { X = 2; Y = 1 }) [||]
            Expect.isTrue (isDocked state) "should be docked at edge"
    ]

[<Tests>]
let checkDockingTests =
    testList "checkDocking" [
        testCase "resupplies energy to 3000 when docked" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } (Some { X = 4; Y = 5 }) [||]
            let state = { state with Enterprise = { state.Enterprise with Energy = 1500.0 } }
            let msgs, newState = checkDocking state
            Expect.equal newState.Enterprise.Energy 3000.0 "energy should be 3000"

        testCase "resupplies torpedoes to 10 when docked" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } (Some { X = 4; Y = 5 }) [||]
            let state = { state with Enterprise = { state.Enterprise with Torpedoes = 3 } }
            let msgs, newState = checkDocking state
            Expect.equal newState.Enterprise.Torpedoes 10 "torpedoes should be 10"

        testCase "drops shields to 0 when docked" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } (Some { X = 4; Y = 5 }) [||]
            let state = { state with Enterprise = { state.Enterprise with Shields = 500.0 } }
            let msgs, newState = checkDocking state
            Expect.equal newState.Enterprise.Shields 0.0 "shields should be 0"

        testCase "shows docking message when docked" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } (Some { X = 4; Y = 5 }) [||]
            let msgs, _ = checkDocking state
            Expect.contains msgs "SHIELDS DROPPED FOR DOCKING PURPOSES" "should show docking message"

        testCase "no changes when not docked" <| fun _ ->
            let state = makeState { X = 4; Y = 4 } (Some { X = 4; Y = 7 }) [||]
            let state = { state with Enterprise = { state.Enterprise with Energy = 1500.0; Torpedoes = 3; Shields = 500.0 } }
            let msgs, newState = checkDocking state
            Expect.isEmpty msgs "no messages when not docked"
            Expect.equal newState.Enterprise.Energy 1500.0 "energy unchanged"
            Expect.equal newState.Enterprise.Torpedoes 3 "torpedoes unchanged"
            Expect.equal newState.Enterprise.Shields 500.0 "shields unchanged"
    ]

[<Tests>]
let dockingProtectionTests =
    testList "docking protection" [
        testCase "klingonAttack still fires when not docked" <| fun _ ->
            let klingons = [| mkKlingon 6 4 200.0 |]
            let state = makeState { X = 4; Y = 4 } None klingons
            let state = { state with Enterprise = { state.Enterprise with Shields = 500.0 } }
            let msgs, newState = klingonAttack state
            Expect.isNonEmpty msgs "should have attack messages"
            Expect.isLessThan newState.Enterprise.Shields 500.0 "shields should decrease"

        testCase "klingonAttack damages shields when not docked near starbase" <| fun _ ->
            let klingons = [| mkKlingon 6 4 200.0 |]
            // Starbase is far away, not adjacent
            let state = makeState { X = 4; Y = 4 } (Some { X = 1; Y = 1 }) klingons
            let state = { state with Enterprise = { state.Enterprise with Shields = 500.0 } }
            let msgs, newState = klingonAttack state
            Expect.isNonEmpty msgs "should have attack messages"
            Expect.isLessThan newState.Enterprise.Shields 500.0 "shields should decrease when not docked"
    ]
