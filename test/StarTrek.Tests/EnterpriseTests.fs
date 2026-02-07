module StarTrek.Tests.EnterpriseTests

open Expecto
open StarTrek.GameTypes
open StarTrek.Enterprise

[<Tests>]
let enterpriseTests =
    testList "Enterprise" [
        testCase "resetEnterprise sets energy shields and torpedoes" <| fun _ ->
            let ship = resetEnterprise { X = 3; Y = 4 } { X = 5; Y = 6 }
            Expect.equal ship.Energy 3000.0 "energy should be 3000"
            Expect.equal ship.Shields 0.0 "shields should be 0"
            Expect.equal ship.Torpedoes 10 "torpedoes should be 10"

        testCase "resetEnterprise sets quadrant and sector positions" <| fun _ ->
            let quadrant = { X = 2; Y = 7 }
            let sector = { X = 4; Y = 1 }
            let ship = resetEnterprise quadrant sector
            Expect.equal ship.Quadrant quadrant "quadrant should match input"
            Expect.equal ship.Sector sector "sector should match input"

        testCase "resetEnterprise initializes all 8 damage systems to zero" <| fun _ ->
            let ship = resetEnterprise { X = 1; Y = 1 } { X = 1; Y = 1 }
            Expect.equal ship.Damage.Length 8 "should have 8 damage entries"
            for d in ship.Damage do
                Expect.equal d.Amount 0 $"damage for {d.System} should be 0"
    ]
