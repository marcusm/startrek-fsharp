module StarTrek.Tests.RepairTests

open Expecto
open StarTrek.GameTypes
open StarTrek.Enterprise
open StarTrek.Galaxy

/// A mock random that returns values from a sequence of doubles
type SequenceRandom(doubles: float list) =
    let mutable remaining = doubles
    interface IRandomService with
        member _.Next() = 0
        member _.Next(_max: int) = 0
        member _.Next(min: int, max: int) =
            let d = remaining.Head
            remaining <- remaining.Tail
            min + int (d * float (max - min))
        member _.NextDouble() =
            let d = remaining.Head
            remaining <- remaining.Tail
            d

let private makeEnterprise () =
    resetEnterprise { X = 4; Y = 4 } { X = 4; Y = 4 }

let private damageSystem (system: SystemDamage) (amount: int) (enterprise: Enterprise) =
    { enterprise with
        Damage = enterprise.Damage |> List.map (fun d ->
            if d.System = system then { d with Amount = amount } else d) }

[<Tests>]
let automaticRepairTests =
    testList "automaticRepair" [
        testCase "repairs each damaged device by +1" <| fun _ ->
            let ship =
                makeEnterprise ()
                |> damageSystem WarpEngines -3
                |> damageSystem Phasers -1
            let repaired = automaticRepair ship
            let warpDmg = repaired.Damage |> List.find (fun d -> d.System = WarpEngines)
            let phaserDmg = repaired.Damage |> List.find (fun d -> d.System = Phasers)
            Expect.equal warpDmg.Amount -2 "warp should go from -3 to -2"
            Expect.equal phaserDmg.Amount 0 "phasers should go from -1 to 0"

        testCase "does not change undamaged devices" <| fun _ ->
            let ship = makeEnterprise () |> damageSystem WarpEngines -2
            let repaired = automaticRepair ship
            let computerDmg = repaired.Damage |> List.find (fun d -> d.System = Computer)
            Expect.equal computerDmg.Amount 0 "undamaged device stays at 0"

        testCase "does not repair above zero" <| fun _ ->
            let ship = makeEnterprise () |> damageSystem Phasers -1
            let repaired = automaticRepair ship
            let phaserDmg = repaired.Damage |> List.find (fun d -> d.System = Phasers)
            Expect.equal phaserDmg.Amount 0 "should stop at 0, not go positive"

        testCase "fully healthy ship is unchanged" <| fun _ ->
            let ship = makeEnterprise ()
            let repaired = automaticRepair ship
            Expect.equal repaired.Damage ship.Damage "no changes to healthy ship"

        testCase "multiple damaged systems all repair by 1" <| fun _ ->
            let ship =
                makeEnterprise ()
                |> damageSystem WarpEngines -5
                |> damageSystem ShortRangeSensors -3
                |> damageSystem Computer -1
            let repaired = automaticRepair ship
            let getDmg sys = repaired.Damage |> List.find (fun d -> d.System = sys) |> fun d -> d.Amount
            Expect.equal (getDmg WarpEngines) -4 "warp: -5 -> -4"
            Expect.equal (getDmg ShortRangeSensors) -2 "SRS: -3 -> -2"
            Expect.equal (getDmg Computer) 0 "computer: -1 -> 0"
    ]

[<Tests>]
let randomDamageEventTests =
    testList "randomDamageEvent" [
        testCase "no event when random > 0.2" <| fun _ ->
            // First NextDouble() call returns 0.5 (> 0.2, so no event)
            let random = SequenceRandom([0.5]) :> IRandomService
            let ship = makeEnterprise ()
            let msgs, result = randomDamageEvent random ship
            Expect.isEmpty msgs "no messages when no event"
            Expect.equal result.Damage ship.Damage "damage unchanged"

        testCase "damage event when random <= 0.2" <| fun _ ->
            // Doubles: 0.1 (trigger), 0.0 (device index 0 = WarpEngines), 0.5 (severity -> 3), 0.3 (< 0.5 = damage)
            let random = SequenceRandom([0.1; 0.0; 0.5; 0.3]) :> IRandomService
            let ship = makeEnterprise ()
            let msgs, result = randomDamageEvent random ship
            let warpDmg = result.Damage |> List.find (fun d -> d.System = WarpEngines)
            Expect.isLessThan warpDmg.Amount 0 "warp engines should be damaged"
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("DAMAGED"))) "should have damage message"

        testCase "improvement event when random >= 0.5" <| fun _ ->
            // Doubles: 0.1 (trigger), 0.0 (device index 0 = WarpEngines), 0.5 (severity -> 3), 0.7 (>= 0.5 = improve)
            let random = SequenceRandom([0.1; 0.0; 0.5; 0.7]) :> IRandomService
            let ship = makeEnterprise () |> damageSystem WarpEngines -5
            let msgs, result = randomDamageEvent random ship
            let warpDmg = result.Damage |> List.find (fun d -> d.System = WarpEngines)
            Expect.isGreaterThan warpDmg.Amount -5 "warp engines should be improved"
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("IMPROVED"))) "should have improvement message"

        testCase "damage message includes device name" <| fun _ ->
            // Doubles: 0.0 (trigger), 0.375 (device index 3 = Phasers), 0.0 (severity -> 1), 0.2 (damage)
            let random = SequenceRandom([0.0; 0.375; 0.0; 0.2]) :> IRandomService
            let ship = makeEnterprise ()
            let msgs, _ = randomDamageEvent random ship
            Expect.isTrue (msgs |> List.exists (fun m -> m.Contains("PHASER CONTROL"))) "should name phaser control"

        testCase "severity ranges from 1 to 5" <| fun _ ->
            // severity = int(0.9 * 5) + 1 = 5
            let random = SequenceRandom([0.0; 0.0; 0.9; 0.0]) :> IRandomService
            let ship = makeEnterprise ()
            let _, result = randomDamageEvent random ship
            let warpDmg = result.Damage |> List.find (fun d -> d.System = WarpEngines)
            Expect.equal warpDmg.Amount -5 "severity 5 damage applied"

        testCase "messages surrounded by blank lines" <| fun _ ->
            let random = SequenceRandom([0.0; 0.0; 0.0; 0.0]) :> IRandomService
            let ship = makeEnterprise ()
            let msgs, _ = randomDamageEvent random ship
            Expect.equal msgs.Length 3 "should have 3 lines (blank, message, blank)"
            Expect.equal msgs.[0] "" "first line blank"
            Expect.equal msgs.[2] "" "last line blank"
    ]

[<Tests>]
let warpRepairIntegrationTests =
    testList "warp repair integration" [
        testCase "warp move repairs damaged devices" <| fun _ ->
            let state = initializeGame 42
            let damagedEnterprise =
                state.Enterprise
                |> damageSystem WarpEngines -3
                |> damageSystem Phasers -2
            let state = { state with Enterprise = damagedEnterprise }
            let state = { state with CurrentQuadrant = Array2D.create 8 8 Empty }
            let ep = state.Enterprise.Sector
            let sectorMap = Array2D.copy state.CurrentQuadrant
            sectorMap.[ep.Y - 1, ep.X - 1] <- Enterprise
            let state = { state with CurrentQuadrant = sectorMap }

            let direction = getCourseVector 1.0 |> Option.get
            let _, newState = executeWarp direction 0.125 state

            let getAmount sys = newState.Enterprise.Damage |> List.find (fun d -> d.System = sys) |> fun d -> d.Amount
            // After automatic repair: WarpEngines -3 -> -2, Phasers -2 -> -1
            // Random event may also apply but we check the automatic repair happened
            Expect.isGreaterThan (getAmount WarpEngines) -3 "warp engines should have repaired"
            Expect.isGreaterThan (getAmount Phasers) -2 "phasers should have repaired"
    ]
