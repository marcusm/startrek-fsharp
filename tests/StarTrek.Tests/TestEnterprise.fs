module TestEnterprise

open Fuchu
open Swensen.Unquote

open StarTrek.GameTypes
open StarTrek.Enterprise

[<Tests>]
let tests =
    TestList [

        testList "Enterprise & Damage" [
            testCase "A fully repaired enterprise does not change state during repairs" <| fun _ ->
                let ship = { sector = {x = 1; y = 1};
                    quadrant = {x = 4; y = 5};
                    energy = 3000.0;
                    shields = 0.0;
                    damage = []}

                test <@ ship= tick ship @>

            testCase "A damaged enterprise will be repaired" <| fun _ ->
                let damage = {amount = -1; system=PhotonTubes} :: []

                let ship = { sector = {x = 1; y = 1};
                    quadrant = {x = 4; y = 5};
                    energy = 3000.0;
                    shields = 0.0;
                    damage = damage }

                test <@ [] = (tick ship).damage @>

            testCase "An improved enterprise will not be harmed" <| fun _ ->
                let damage = {amount = 3; system=PhotonTubes} :: []

                let ship = { sector = {x = 1; y = 1};
                    quadrant = {x = 4; y = 5};
                    energy = 3000.0;
                    shields = 0.0;
                    damage = damage }

                test <@ ship = tick ship @>
        ]
    ]

