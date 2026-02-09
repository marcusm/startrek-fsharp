module StarTrek.Tests.GameEndTests

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

let private makeState (klingons: Klingon array) enterpriseSector energy shields stardate turns =
    let enterprise =
        { (resetEnterprise { X = 4; Y = 4 } enterpriseSector) with
            Energy = energy
            Shields = shields }
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
      Stardate = { Current = stardate; Start = 2500; Turns = turns }
      QuadrantsScanned = Set.empty
      Random = FixedRandom(1.0) :> IRandomService
      InitialKlingons = 15 }

[<Tests>]
let checkGameEndTests =
    testList "checkGameEnd" [
        testCase "returns Victory when no Klingons remain in galaxy" <| fun _ ->
            let state = makeState [||] { X = 4; Y = 4 } 3000.0 0.0 2505 30
            Expect.equal (checkGameEnd state) (Some Victory) "should be Victory when 0 klingons"

        testCase "returns None when Klingons still exist" <| fun _ ->
            let state = makeState [| mkKlingon 2 2 200.0 |] { X = 4; Y = 4 } 3000.0 0.0 2505 30
            Expect.equal (checkGameEnd state) None "should be None with klingons present"

        testCase "returns TimeExpired when stardates used up" <| fun _ ->
            // Start=2500, Current=2530, Turns=30 → remaining = 30-(2530-2500) = 0
            let state = makeState [| mkKlingon 2 2 200.0 |] { X = 4; Y = 4 } 3000.0 0.0 2530 30
            Expect.equal (checkGameEnd state) (Some TimeExpired) "should be TimeExpired"

        testCase "returns TimeExpired when stardates exceeded" <| fun _ ->
            // Start=2500, Current=2535, Turns=30 → remaining = 30-(2535-2500) = -5
            let state = makeState [| mkKlingon 2 2 200.0 |] { X = 4; Y = 4 } 3000.0 0.0 2535 30
            Expect.equal (checkGameEnd state) (Some TimeExpired) "should be TimeExpired when exceeded"

        testCase "returns None when stardates remain" <| fun _ ->
            let state = makeState [| mkKlingon 2 2 200.0 |] { X = 4; Y = 4 } 3000.0 0.0 2520 30
            Expect.equal (checkGameEnd state) None "should be None with stardates remaining"

        testCase "returns PlayerDestroyed when shields negative" <| fun _ ->
            let state = makeState [| mkKlingon 2 2 200.0 |] { X = 4; Y = 4 } 100.0 -10.0 2505 30
            Expect.equal (checkGameEnd state) (Some PlayerDestroyed) "should be PlayerDestroyed"

        testCase "returns PlayerDeadInSpace when energy depleted with Klingons" <| fun _ ->
            // DeadInSpace: shields > 0 && shields < 1.1 && energy = 0
            let state = makeState [| mkKlingon 2 2 200.0 |] { X = 4; Y = 4 } 0.0 1.0 2505 30
            Expect.equal (checkGameEnd state) (Some PlayerDeadInSpace) "should be PlayerDeadInSpace"

        testCase "returns None for DeadInSpace without Klingons in quadrant" <| fun _ ->
            // DeadInSpace condition but no Klingons in current quadrant
            let state = makeState [||] { X = 4; Y = 4 } 0.0 1.0 2505 30
            // No Klingons anywhere = Victory takes priority
            Expect.equal (checkGameEnd state) (Some Victory) "Victory takes priority over DeadInSpace"

        testCase "Victory takes priority over TimeExpired" <| fun _ ->
            // No Klingons AND time expired → Victory wins
            let state = makeState [||] { X = 4; Y = 4 } 3000.0 0.0 2530 30
            Expect.equal (checkGameEnd state) (Some Victory) "Victory should take priority"
    ]

[<Tests>]
let totalKlingonsRemainingTests =
    testList "totalKlingonsRemaining" [
        testCase "counts Klingons across all quadrants" <| fun _ ->
            let state = makeState [| mkKlingon 2 2 200.0 |] { X = 4; Y = 4 } 3000.0 0.0 2505 30
            // One Klingon in quadrant (4,4)
            Expect.equal (totalKlingonsRemaining state) 1 "should count 1 Klingon"

        testCase "returns zero when no Klingons" <| fun _ ->
            let state = makeState [||] { X = 4; Y = 4 } 3000.0 0.0 2505 30
            Expect.equal (totalKlingonsRemaining state) 0 "should count 0 Klingons"
    ]

[<Tests>]
let stardatesRemainingTests =
    testList "stardatesRemaining" [
        testCase "calculates remaining stardates correctly" <| fun _ ->
            let state = makeState [||] { X = 4; Y = 4 } 3000.0 0.0 2510 30
            // Turns=30, Current=2510, Start=2500 → remaining = 30 - 10 = 20
            Expect.equal (stardatesRemaining state) 20 "should have 20 stardates left"

        testCase "returns zero when exactly at limit" <| fun _ ->
            let state = makeState [||] { X = 4; Y = 4 } 3000.0 0.0 2530 30
            Expect.equal (stardatesRemaining state) 0 "should have 0 stardates left"

        testCase "returns negative when past limit" <| fun _ ->
            let state = makeState [||] { X = 4; Y = 4 } 3000.0 0.0 2535 30
            Expect.equal (stardatesRemaining state) -5 "should be -5 stardates"
    ]

[<Tests>]
let efficiencyRatingTests =
    testList "efficiencyRating" [
        testCase "calculates rating as initialKlingons / timeUsed * 1000" <| fun _ ->
            let state =
                { (makeState [||] { X = 4; Y = 4 } 3000.0 0.0 2510 30) with
                    InitialKlingons = 15 }
            // timeUsed = 2510 - 2500 = 10, rating = 15/10 * 1000 = 1500
            Expect.floatClose Accuracy.medium (efficiencyRating state) 1500.0 "rating should be 1500"

        testCase "handles zero time used" <| fun _ ->
            let state =
                { (makeState [||] { X = 4; Y = 4 } 3000.0 0.0 2500 30) with
                    InitialKlingons = 15 }
            // timeUsed = 0, rating = initialKlingons * 1000
            Expect.floatClose Accuracy.medium (efficiencyRating state) 15000.0 "should handle zero time"

        testCase "higher time produces lower rating" <| fun _ ->
            let state1 =
                { (makeState [||] { X = 4; Y = 4 } 3000.0 0.0 2505 30) with
                    InitialKlingons = 15 }
            let state2 =
                { (makeState [||] { X = 4; Y = 4 } 3000.0 0.0 2520 30) with
                    InitialKlingons = 15 }
            Expect.isGreaterThan (efficiencyRating state1) (efficiencyRating state2) "faster = better rating"
    ]

[<Tests>]
let victoryMessagesTests =
    testList "victoryMessages" [
        testCase "contains victory announcement" <| fun _ ->
            let state =
                { (makeState [||] { X = 4; Y = 4 } 3000.0 0.0 2510 30) with
                    InitialKlingons = 15 }
            let msgs = victoryMessages state
            Expect.isTrue
                (msgs |> List.exists (fun m -> m.Contains("LAST KLINGON BATTLE CRUISER")))
                "should contain victory text"

        testCase "contains promotion message" <| fun _ ->
            let state =
                { (makeState [||] { X = 4; Y = 4 } 3000.0 0.0 2510 30) with
                    InitialKlingons = 15 }
            let msgs = victoryMessages state
            Expect.isTrue
                (msgs |> List.exists (fun m -> m.Contains("PROMOTED TO ADMIRAL")))
                "should contain promotion text"

        testCase "contains efficiency rating" <| fun _ ->
            let state =
                { (makeState [||] { X = 4; Y = 4 } 3000.0 0.0 2510 30) with
                    InitialKlingons = 15 }
            let msgs = victoryMessages state
            Expect.isTrue
                (msgs |> List.exists (fun m -> m.Contains("EFFICIENCY RATING")))
                "should contain efficiency rating"
    ]

[<Tests>]
let timeExpiredMessagesTests =
    testList "timeExpiredMessages" [
        testCase "shows current stardate" <| fun _ ->
            let state = makeState [| mkKlingon 2 2 200.0 |] { X = 4; Y = 4 } 3000.0 0.0 2530 30
            let msgs = timeExpiredMessages state
            Expect.isTrue
                (msgs |> List.exists (fun m -> m.Contains("2530")))
                "should show current stardate"

        testCase "shows remaining Klingon count" <| fun _ ->
            let state = makeState [| mkKlingon 2 2 200.0 |] { X = 4; Y = 4 } 3000.0 0.0 2530 30
            let msgs = timeExpiredMessages state
            Expect.isTrue
                (msgs |> List.exists (fun m -> m.Contains("1 KLINGON")))
                "should show klingon count"

        testCase "contains relieved of command" <| fun _ ->
            let state = makeState [| mkKlingon 2 2 200.0 |] { X = 4; Y = 4 } 3000.0 0.0 2530 30
            let msgs = timeExpiredMessages state
            Expect.isTrue
                (msgs |> List.exists (fun m -> m.Contains("RELIEVED OF COMMAND")))
                "should contain relieved message"
    ]

[<Tests>]
let destroyedMessagesTests =
    testList "destroyedMessages" [
        testCase "contains destruction announcement" <| fun _ ->
            let msgs = destroyedMessages ()
            Expect.isTrue
                (msgs |> List.exists (fun m -> m.Contains("ENTERPRISE HAS BEEN DESTROYED")))
                "should contain destroyed text"
    ]

[<Tests>]
let deadInSpaceMessagesTests =
    testList "deadInSpaceMessages" [
        testCase "contains dead in space announcement" <| fun _ ->
            let msgs = deadInSpaceMessages ()
            Expect.isTrue
                (msgs |> List.exists (fun m -> m.Contains("DEAD IN SPACE")))
                "should contain dead in space text"

        testCase "contains demotion" <| fun _ ->
            let msgs = deadInSpaceMessages ()
            Expect.isTrue
                (msgs |> List.exists (fun m -> m.Contains("DEMOTED")))
                "should contain demotion text"
    ]
