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

        testCase "isShieldControlDamaged returns false when undamaged" <| fun _ ->
            let ship = resetEnterprise { X = 1; Y = 1 } { X = 1; Y = 1 }
            Expect.isFalse (isShieldControlDamaged ship) "should not be damaged"

        testCase "isShieldControlDamaged returns true when damaged" <| fun _ ->
            let ship = resetEnterprise { X = 1; Y = 1 } { X = 1; Y = 1 }
            let damaged = { ship with Damage = ship.Damage |> List.map (fun d -> if d.System = ShieldControl then { d with Amount = -1 } else d) }
            Expect.isTrue (isShieldControlDamaged damaged) "should be damaged"

        testCase "transferShields sets shields to requested level" <| fun _ ->
            let ship = { (resetEnterprise { X = 1; Y = 1 } { X = 1; Y = 1 }) with Energy = 3000.0; Shields = 0.0 }
            match transferShields 500.0 ship with
            | Ok result ->
                Expect.equal result.Shields 500.0 "shields should be 500"
                Expect.equal result.Energy 2500.0 "energy should be 2500"
            | Error msg -> failtest msg

        testCase "transferShields redistributes between shields and energy" <| fun _ ->
            let ship = { (resetEnterprise { X = 1; Y = 1 } { X = 1; Y = 1 }) with Energy = 2000.0; Shields = 500.0 }
            match transferShields 800.0 ship with
            | Ok result ->
                Expect.equal result.Shields 800.0 "shields should be set to 800"
                Expect.equal result.Energy 1700.0 "energy should be total (2500) - 800"
            | Error msg -> failtest msg

        testCase "transferShields can lower shields returning energy" <| fun _ ->
            let ship = { (resetEnterprise { X = 1; Y = 1 } { X = 1; Y = 1 }) with Energy = 2000.0; Shields = 500.0 }
            match transferShields 300.0 ship with
            | Ok result ->
                Expect.equal result.Shields 300.0 "shields should be lowered to 300"
                Expect.equal result.Energy 2200.0 "200 units should return to energy pool (2500 - 300)"
            | Error msg -> failtest msg

        testCase "transferShields over total energy returns treasury error" <| fun _ ->
            let ship = { (resetEnterprise { X = 1; Y = 1 } { X = 1; Y = 1 }) with Energy = 3000.0; Shields = 0.0 }
            match transferShields 4000.0 ship with
            | Error msg -> Expect.stringContains msg "NOT THE FEDERATION TREASURY" "should mention treasury"
            | Ok _ -> failtest "should have returned error"

        testCase "transferShields negative returns invalid error" <| fun _ ->
            let ship = resetEnterprise { X = 1; Y = 1 } { X = 1; Y = 1 }
            match transferShields -100.0 ship with
            | Error msg -> Expect.stringContains msg "INVALID" "should say invalid"
            | Ok _ -> failtest "should have returned error"

        testCase "transferShields equal to total energy sets energy to zero" <| fun _ ->
            let ship = { (resetEnterprise { X = 1; Y = 1 } { X = 1; Y = 1 }) with Energy = 2000.0; Shields = 1000.0 }
            match transferShields 3000.0 ship with
            | Ok result ->
                Expect.equal result.Shields 3000.0 "shields should be 3000"
                Expect.equal result.Energy 0.0 "energy should be 0"
            | Error msg -> failtest msg
    ]
