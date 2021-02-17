namespace Tests.StartTrek

module ``Enterprise Tests`` =

    open System
    open Xunit
    open FsUnit.Xunit
    open Moq
    open StarTrek.GameTypes
    open StarTrek.Enterprise

    type ``Given a damaged enterprise`` () =
        let damage = {Amount = -4; System=PhotonTubes} :: []

        let ship = { Sector = {X = 1; Y = 1};
            Quadrant = {X = 4; Y = 5};
            Energy = 3000.0;
            Shields = 0.0;
            Torpedoes = 10;
            Damage = damage }

        [<Fact>]
        member x.
            `` A heavily damaged enterprise is not completely repaired `` () =
                let random = Mock<IRandomService>();
                random.Setup(fun r -> r.NextDouble()).Returns(0.6) |> ignore;
                let e = tick random.Object ship
                e.Damage.Head |> should equal { Amount = -3; System = PhotonTubes }

        [<Fact>]
        member x.
            `` An improved enterprise will not be harmed`` () =
                let improved = {Amount = -4; System=PhotonTubes} :: []
                let ship = { ship with Damage = improved }
                let random = Mock<IRandomService>();
                random.Setup(fun r -> r.NextDouble()).Returns(0.6) |> ignore;

                let e = tick random.Object ship
                e.Damage.Head |> should equal { Amount = -3; System = PhotonTubes }
                