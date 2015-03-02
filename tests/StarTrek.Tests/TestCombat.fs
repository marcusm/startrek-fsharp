module TestCombat

open Fuchu
open Swensen.Unquote

open StarTrek.GameTypes
open StarTrek.Enterprise

[<Tests>]
let tests =
    TestList [
        testList "Simple List" [
            testCase "When 2 is added to 2 expect 4" <|
                fun _ -> test <@ 2 + 2 = 4 @>
        ]
    ]
