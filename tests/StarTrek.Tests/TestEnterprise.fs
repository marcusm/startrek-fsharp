module TestEnterprise

open Fuchu
open Swensen.Unquote

open StarTrek.GameTypes
open StarTrek.Enterprise

[<Tests>]
let tests =
    TestList [

        testList "Enterprise & Damage" [
            testCase "Can repair a single system." <| fun _ ->
                let damage = {amount = -4; system=PhotonTubes}
                let damage2 = {amount = -3; system=PhotonTubes}

                test <@ damage2 = damage.repairSystem @>

            testCase "A fully repaired enterprise does not change state during repairs" <| fun _ ->
                let ship = { sector = {x = 1; y = 1};
                    quadrant = {x = 4; y = 5};
                    energy = 3000.0;
                    shields = 0.0;
                    torpedoes = 10;
                    damage = []}

                test <@ ship= tick ship @>

            testCase "A heavily damaged enterprise is not completely repaired" <| fun _ ->
                let damage = {amount = -4; system=PhotonTubes} :: []
                let damage2 = {amount = -3; system=PhotonTubes} :: []

                let ship = { sector = {x = 1; y = 1};
                    quadrant = {x = 4; y = 5};
                    energy = 3000.0;
                    shields = 0.0;
                    torpedoes = 10;
                    damage = damage }

                test <@ damage2 = repairSystems ship @>

            testCase "A damaged enterprise will be repaired" <| fun _ ->
                let damage = {amount = -1; system=PhotonTubes} :: []

                let ship = { sector = {x = 1; y = 1};
                    quadrant = {x = 4; y = 5};
                    energy = 3000.0;
                    shields = 0.0;
                    torpedoes = 10;
                    damage = damage }

                test <@ [] = (tick ship).damage @>

            testCase "An improved enterprise will not be harmed" <| fun _ ->
                let damage = {amount = 3; system=PhotonTubes} :: []

                let ship = { sector = {x = 1; y = 1};
                    quadrant = {x = 4; y = 5};
                    energy = 3000.0;
                    shields = 0.0;
                    torpedoes = 10;
                    damage = damage }

                test <@ ship = tick ship @>
        ]
    ]

