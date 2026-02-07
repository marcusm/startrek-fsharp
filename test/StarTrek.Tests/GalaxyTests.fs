module StarTrek.Tests.GalaxyTests

open Expecto
open StarTrek
open StarTrek.Galaxy

[<Tests>]
let galaxyTests =
    testList "Galaxy" [
        testCase "encodeQuadrant encodes klingons starbases and stars" <| fun _ ->
            let info = { Klingons = 3; Starbases = 1; Stars = 5 }
            Expect.equal (encodeQuadrant info) 315 "should encode as 315"

        testCase "decodeQuadrant reverses encodeQuadrant" <| fun _ ->
            let info = { Klingons = 2; Starbases = 0; Stars = 7 }
            let decoded = decodeQuadrant (encodeQuadrant info)
            Expect.equal decoded.Klingons info.Klingons "klingons should match"
            Expect.equal decoded.Starbases info.Starbases "starbases should match"
            Expect.equal decoded.Stars info.Stars "stars should match"

        testCase "encodeQuadrant with zeros returns zero" <| fun _ ->
            let info = { Klingons = 0; Starbases = 0; Stars = 0 }
            Expect.equal (encodeQuadrant info) 0 "should encode as 0"

        testCase "isValidPosition accepts positions within bounds" <| fun _ ->
            Expect.isTrue (isValidPosition { X = 0; Y = 0 }) "origin is valid"
            Expect.isTrue (isValidPosition { X = 7; Y = 7 }) "max corner is valid"
            Expect.isTrue (isValidPosition { X = 3; Y = 5 }) "interior is valid"

        testCase "isValidPosition rejects positions out of bounds" <| fun _ ->
            Expect.isFalse (isValidPosition { X = -1; Y = 0 }) "negative X is invalid"
            Expect.isFalse (isValidPosition { X = 0; Y = -1 }) "negative Y is invalid"
            Expect.isFalse (isValidPosition { X = 8; Y = 0 }) "X=8 is invalid"
            Expect.isFalse (isValidPosition { X = 0; Y = 8 }) "Y=8 is invalid"

        testCase "sectorToChar returns correct symbols" <| fun _ ->
            Expect.equal (sectorToChar Enterprise) "<*>" "enterprise"
            Expect.equal (sectorToChar (Klingon 200.0)) "+++" "klingon"
            Expect.equal (sectorToChar Starbase) ">!<" "starbase"
            Expect.equal (sectorToChar Star) " * " "star"
            Expect.equal (sectorToChar Empty) "   " "empty"

        testCase "getCourseVector returns None for invalid course" <| fun _ ->
            Expect.isNone (getCourseVector 0.5) "below 1 is invalid"
            Expect.isNone (getCourseVector 9.0) "9.0 is invalid"

        testCase "getCourseVector returns Some for valid course" <| fun _ ->
            Expect.isSome (getCourseVector 1.0) "1.0 is valid"
            Expect.isSome (getCourseVector 4.5) "4.5 is valid"
            Expect.isSome (getCourseVector 8.99) "8.99 is valid"

        testCase "createEmptySectorMap creates 8x8 grid of Empty" <| fun _ ->
            let map = createEmptySectorMap ()
            Expect.equal (Array2D.length1 map) 8 "8 rows"
            Expect.equal (Array2D.length2 map) 8 "8 columns"
            for y in 0..7 do
                for x in 0..7 do
                    Expect.equal map.[y, x] Empty $"cell [{y},{x}] should be Empty"
    ]
