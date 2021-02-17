namespace StarTrek

module GameTypes =
    let dim = 8

    let enterpriseId = 1
    let baseId = 2
    let klingonId = 3
    let starId = 4

    type IRandomService =
        abstract member Next: unit->int
        abstract member Next: int->int
        abstract member Next: int*int->int
        abstract member NextDouble: unit->double

    type Point = { X: int; Y: int }

    type Quadrant =
        { Quadrant: Point
          Bases: int
          Klingons: int
          Stars: int }

    type Stardate =
        { Current: int
          Start: int
          Turns: int }

    type QuadrantClutter = Map<Point, int>

    type ShipCondition =
        | Operational
        | DeadInSpace
        | Destroyed

    type Klingon =
        { Sector: Point
          Energy: float }

        member this.ShipCondition =
            match this.Energy with
            | e when e < 0.0 -> ShipCondition.Destroyed
            | _ -> ShipCondition.Operational

    type SystemDamage =
        | Computer
        | WarpEngines
        | ShortRangeSensors
        | LongRangeSensors
        | Phasers
        | PhotonTubes
        | DamageControl
        | ShieldControl

    let randInst<'T> (random:IRandomService) =
        let cases = Reflection.FSharpType.GetUnionCases(typeof<'T>)

        let index = random.Next(cases.Length)
        let case = cases.[index]
        Reflection.FSharpValue.MakeUnion(case, [||]) :?> 'T

    type Damage =
        { Amount: int
          System: SystemDamage }
        member this.RepairSystem =
            let { Amount = x; System = s } = this

            match x with
            | (a) when a < 0 -> { Amount = min 0 (x + 1); System = s }
            | _ -> this

    type Enterprise =
        { Sector: Point
          Quadrant: Point
          Energy: float
          Shields: float
          Torpedoes: int
          Damage: List<Damage> }

    type GameState =
        { Enterprise: Enterprise
          Klingons: Klingon []
          CurrentQuadrant: int [,]
          Quadrants: Quadrant [,]
          Stardate: Stardate }