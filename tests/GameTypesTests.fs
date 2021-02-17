namespace Tests.StartTrek

module ``GameTypes Tests`` =

    open System
    open Xunit
    open FsUnit.Xunit
    open StarTrek.GameTypes

    let damage = { Amount = -4; System = PhotonTubes }

    [<Fact>]
    let ``Given a damaged system, repair the system one step`` () =
        damage.RepairSystem
        |> should equal { Amount = -3; System = PhotonTubes }

    [<Fact>]
    let ``Given a damaged system, repair the system to full`` () =
        let damage = damage.RepairSystem
        let damage = damage.RepairSystem
        let damage = damage.RepairSystem

        damage.RepairSystem
        |> should equal { Amount = 0; System = PhotonTubes }

    [<Fact>]
    let ``Given a working system, repairing the system has no effect`` () =
        let working =
            { Amount = 0; System = Phasers }.RepairSystem

        working.RepairSystem |> should equal working
