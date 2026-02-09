module StarTrek.Tests.TorpedoTests

open Expecto
open StarTrek.GameTypes
open StarTrek.Enterprise
open StarTrek.Galaxy

type FixedRandom(nextIntValue: int, nextDoubleValue: float) =
    interface IRandomService with
        member _.Next() = 0
        member _.Next(_max: int) = nextIntValue
        member _.Next(min: int, _max: int) = min + nextIntValue
        member _.NextDouble() = nextDoubleValue

let private mkKlingon x y energy : Klingon =
    { Sector = { X = x; Y = y }; Energy = energy }

let private makeState (klingons: Klingon array) enterpriseSector (random: IRandomService) =
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
      Random = random
      InitialKlingons = Array.length klingons }

let private makeStateWithStarbase enterpriseSector starbasePos (random: IRandomService) =
    let enterprise = resetEnterprise { X = 4; Y = 4 } enterpriseSector
    let sectorMap = Array2D.create 8 8 Empty
    sectorMap.[enterpriseSector.Y - 1, enterpriseSector.X - 1] <- Sector.Enterprise
    sectorMap.[starbasePos.Y - 1, starbasePos.X - 1] <- Starbase
    let quadrants = Array2D.init 8 8 (fun x y ->
        { Quadrant = { X = x + 1; Y = y + 1 }; Klingons = 0; Starbases = 0; Stars = 4 })
    let qx = enterprise.Quadrant.X - 1
    let qy = enterprise.Quadrant.Y - 1
    quadrants.[qx, qy] <- { quadrants.[qx, qy] with Starbases = 1 }
    { Enterprise = enterprise
      Klingons = [||]
      CurrentQuadrant = sectorMap
      Quadrants = quadrants
      Stardate = { Current = 2500; Start = 2500; Turns = 30 }
      QuadrantsScanned = Set.empty
      Random = random
      InitialKlingons = 0 }

let private makeStateWithStar enterpriseSector starPos (random: IRandomService) =
    let enterprise = resetEnterprise { X = 4; Y = 4 } enterpriseSector
    let sectorMap = Array2D.create 8 8 Empty
    sectorMap.[enterpriseSector.Y - 1, enterpriseSector.X - 1] <- Sector.Enterprise
    sectorMap.[starPos.Y - 1, starPos.X - 1] <- Star
    let quadrants = Array2D.init 8 8 (fun x y ->
        { Quadrant = { X = x + 1; Y = y + 1 }; Klingons = 0; Starbases = 0; Stars = 4 })
    { Enterprise = enterprise
      Klingons = [||]
      CurrentQuadrant = sectorMap
      Quadrants = quadrants
      Stardate = { Current = 2500; Start = 2500; Turns = 30 }
      QuadrantsScanned = Set.empty
      Random = random
      InitialKlingons = 0 }

[<Tests>]
let torpedoDamageCheckTests =
    testList "Torpedo damage checks" [
        testCase "isPhotonTubesDamaged returns false when undamaged" <| fun _ ->
            let ship = resetEnterprise { X = 4; Y = 4 } { X = 1; Y = 1 }
            Expect.isFalse (isPhotonTubesDamaged ship) "should not be damaged"

        testCase "isPhotonTubesDamaged returns true when damaged" <| fun _ ->
            let ship = resetEnterprise { X = 4; Y = 4 } { X = 1; Y = 1 }
            let damaged = { ship with Damage = ship.Damage |> List.map (fun d -> if d.System = PhotonTubes then { d with Amount = -3 } else d) }
            Expect.isTrue (isPhotonTubesDamaged damaged) "should be damaged"
    ]

[<Tests>]
let firePhotonTorpedoTests =
    testList "firePhotonTorpedo" [
        testCase "decrements torpedo count" <| fun _ ->
            let random = FixedRandom(50, 1.0) :> IRandomService
            let state = makeState [| mkKlingon 4 2 200.0 |] { X = 4; Y = 4 } random
            let direction = getCourseVector 3.0 |> Option.get  // North: dy=-1, dx=0
            let _, newState = firePhotonTorpedo direction state
            Expect.equal newState.Enterprise.Torpedoes 9 "torpedo count should be decremented"

        testCase "destroys Klingon when damage exceeds energy" <| fun _ ->
            // FixedRandom Next(100) returns 0+50=50, so damage = 280+50 = 330
            let random = FixedRandom(50, 1.0) :> IRandomService
            let state = makeState [| mkKlingon 4 2 200.0 |] { X = 4; Y = 4 } random
            let direction = getCourseVector 3.0 |> Option.get  // North
            let msgs, newState = firePhotonTorpedo direction state
            Expect.equal newState.Klingons.Length 0 "klingon should be destroyed"
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("KLINGON AT SECTOR") && m.Contains("DESTROYED"))) "should report destruction"
            Expect.equal newState.CurrentQuadrant.[1, 3] Empty "sector should be empty"

        testCase "destroyed Klingon decrements quadrant count" <| fun _ ->
            let random = FixedRandom(50, 1.0) :> IRandomService
            let state = makeState [| mkKlingon 4 2 200.0 |] { X = 4; Y = 4 } random
            let direction = getCourseVector 3.0 |> Option.get
            let _, newState = firePhotonTorpedo direction state
            let qx = newState.Enterprise.Quadrant.X - 1
            let qy = newState.Enterprise.Quadrant.Y - 1
            Expect.equal newState.Quadrants.[qx, qy].Klingons 0 "quadrant klingon count should be 0"

        testCase "Klingon survives when energy exceeds damage" <| fun _ ->
            // damage = 280 + 50 = 330, Klingon has 500
            let random = FixedRandom(50, 1.0) :> IRandomService
            let state = makeState [| mkKlingon 4 2 500.0 |] { X = 4; Y = 4 } random
            let direction = getCourseVector 3.0 |> Option.get
            let msgs, newState = firePhotonTorpedo direction state
            Expect.equal newState.Klingons.Length 1 "klingon should survive"
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("SENSORS SHOW"))) "should show remaining energy"
            Expect.floatClose Accuracy.medium newState.Klingons.[0].Energy 170.0 "energy should be 500-330=170"

        testCase "torpedo misses when exiting sector bounds" <| fun _ ->
            // Enterprise at 4,1, fire North (course 3): next step is 4,0 which is out of bounds
            let random = FixedRandom(50, 1.0) :> IRandomService
            let state = makeState [||] { X = 4; Y = 1 } random
            let direction = getCourseVector 3.0 |> Option.get
            let msgs, _ = firePhotonTorpedo direction state
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("TORPEDO MISSED"))) "should report miss"

        testCase "torpedo reports track for each step" <| fun _ ->
            // Enterprise at 4,4, fire North. Track should show 4,3 then 4,2 (klingon)
            let random = FixedRandom(50, 1.0) :> IRandomService
            let state = makeState [| mkKlingon 4 2 200.0 |] { X = 4; Y = 4 } random
            let direction = getCourseVector 3.0 |> Option.get
            let msgs, _ = firePhotonTorpedo direction state
            let trackMsgs = msgs |> List.filter (fun m -> m.Contains("TORPEDO TRACK"))
            Expect.isTrue (trackMsgs.Length >= 1) "should have track messages"
            Expect.isTrue (trackMsgs.[0].Contains("4,3")) "first track should be 4,3"

        testCase "torpedo destroys starbase" <| fun _ ->
            // Enterprise at 4,4, starbase at 4,2, fire North
            let random = FixedRandom(50, 1.0) :> IRandomService
            let state = makeStateWithStarbase { X = 4; Y = 4 } { X = 4; Y = 2 } random
            let direction = getCourseVector 3.0 |> Option.get
            let msgs, newState = firePhotonTorpedo direction state
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("STAR BASE DESTROYED"))) "should report starbase destruction"
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("CONGRATULATIONS"))) "should congratulate"
            Expect.equal newState.CurrentQuadrant.[1, 3] Empty "starbase sector should be empty"
            let qx = newState.Enterprise.Quadrant.X - 1
            let qy = newState.Enterprise.Quadrant.Y - 1
            Expect.equal newState.Quadrants.[qx, qy].Starbases 0 "starbase count should be decremented"

        testCase "torpedo hits star - rejected with message" <| fun _ ->
            let random = FixedRandom(0, 0.3) :> IRandomService
            let state = makeStateWithStar { X = 4; Y = 4 } { X = 4; Y = 2 } random
            let direction = getCourseVector 3.0 |> Option.get
            let msgs, _ = firePhotonTorpedo direction state
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("YOU CAN'T DESTROY STARS SILLY"))) "should reject with silly message"
    ]

[<Tests>]
let torpedoDataTests =
    testList "torpedoDataLines targeting accuracy" [
        testCase "torpedo data courses accurately target klingons" <| fun _ ->
            let enterprisePos = { X = 4; Y = 4 }

            // Use fixed positions along cardinal/diagonal directions from Enterprise
            // so computed courses align perfectly with the grid
            let klingonPositions = [
                { X = 7; Y = 4 }  // due East (course 1.0)
                { X = 4; Y = 1 }  // due North (course 3.0)
                { X = 1; Y = 7 }  // SW diagonal (course 6.0)
            ]

            let klingons =
                klingonPositions
                |> List.map (fun p -> mkKlingon p.X p.Y 200.0)
                |> List.toArray

            // FixedRandom(50, _): torpedo damage = 280 + 50 = 330 > 200, so every klingon is destroyed
            let random = FixedRandom(50, 1.0) :> IRandomService
            let state = makeState klingons enterprisePos random

            // Use computer option 2 to get torpedo data
            let lines = torpedoDataLines state

            // Parse course values from lines like "  KLINGON AT SECTOR 5,3: COURSE = 2.4  DISTANCE = 3.2"
            let parseCourse (line: string) =
                let m = System.Text.RegularExpressions.Regex.Match(line, @"COURSE = ([\d.]+)")
                if m.Success then Some (float m.Groups.[1].Value)
                else None

            let courses = lines |> List.choose parseCourse
            Expect.equal courses.Length 3
                (sprintf "should find 3 courses in torpedo data. Lines: %A" lines)

            // Fire torpedoes using computed courses, verify each hits something
            let _, allResults =
                courses |> List.fold (fun (st, results) course ->
                    let direction = getCourseVector course |> Option.get
                    let msgs, newState = firePhotonTorpedo direction st
                    let isHit =
                        msgs |> List.exists (fun m ->
                            (m.Contains("KLINGON AT SECTOR") && m.Contains("DESTROYED")) ||
                            m.Contains("SENSORS SHOW") ||
                            m.Contains("STAR BASE DESTROYED") ||
                            m.Contains("YOU CAN'T DESTROY STARS SILLY"))
                    (newState, results @ [(course, isHit, msgs)])
                ) (state, [])

            for (course, isHit, msgs) in allResults do
                Expect.isTrue isHit
                    (sprintf "torpedo at course %.1f should hit. Klingon positions: %A. Messages: %A"
                        course klingonPositions msgs)
    ]
