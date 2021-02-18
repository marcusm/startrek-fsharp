namespace Tests.StartTrek

module ``Utils Tests`` =

    open System
    open Xunit
    open FsUnit.Xunit
    open Moq
    open StarTrek.GameTypes
    open StarTrek.Utils

    [<Fact>]
    let ``Verify that getting a random system works`` () =
        let random = Mock<IRandomService>()

        random
            .Setup(fun r -> r.Next(It.IsAny<int>()))
            .Returns(4)
        |> ignore

        getRandomSystem random.Object
        |> should equal Phasers

    [<Fact>]
    let ``Verify that getRandomCoord works`` () =
        let random = Mock<IRandomService>()

        random
            .SetupSequence(fun r -> r.Next(It.Is<int>(fun i -> i = 1), It.Is<int>(fun i -> i = dim + 1)))
            .Returns(2)
            .Returns(3)
            .Throws(InvalidOperationException())
        |> ignore

        getRandomCoord random.Object Map.empty
        |> should equal { X = 2; Y = 3 }

        random.VerifyAll()

    [<Fact>]
    let ``Verify that getRandomCoord recurses to get unique point`` () =
        let random = Mock<IRandomService>()

        random
            .SetupSequence(fun r -> r.Next(It.Is<int>(fun i -> i = 1), It.Is<int>(fun i -> i = dim + 1)))
            .Returns(2)
            .Returns(3)
            .Returns(4)
            .Returns(5)
            .Throws(InvalidOperationException())
        |> ignore

        let clutter = Map.add { X = 2; Y = 3 } 3 Map.empty

        getRandomCoord random.Object clutter
        |> should equal { X = 4; Y = 5 }

        random.VerifyAll()

    [<Fact>]
    let ``Verify euclidean distance is correct`` () =
        let e = { X = 4; Y = 5 }
        let k = { X = 1; Y = 3 }

        euclidean e k |> should be (inRange 3.6055 3.6056)
