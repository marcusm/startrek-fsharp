module TestWorld

open Fuchu
open Swensen.Unquote

open StarTrek.GameTypes
open StarTrek.World

[<Tests>]
let tests =
    TestList [
        testList "Init World List" [
            testCase "place an item" <| fun _ -> 
                let field = Array2D.zeroCreateBased 1 1 dim dim
                let random = System.Random()
                placeQuadrantItem random field 5 3
                placeQuadrantItem random field 1 7

                test <@ 5 * 3 + 7 = (field |> Seq.cast<int> |> Seq.sum) @>
        ]
    ]

