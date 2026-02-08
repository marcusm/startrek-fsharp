module StarTrek.GameTypes

type IRandomService =
    abstract member Next: unit -> int
    abstract member Next: int -> int
    abstract member Next: int * int -> int
    abstract member NextDouble: unit -> double

type Random(seed: int) =
    let rand = System.Random(seed)

    interface IRandomService with
        member this.Next() = rand.Next()
        member this.Next(max: int) = rand.Next(max)
        member this.Next(min: int, max: int) = rand.Next(min, max)
        member this.NextDouble() = rand.NextDouble()

type Position = { X: int; Y: int }

type Sector =
    | Empty
    | Star
    | Klingon of shieldPower: float
    | Starbase
    | Enterprise

type Quadrant = {
    Quadrant: Position
    Klingons: int
    Starbases: int
    Stars: int
}

type Stardate = {
    Current: int
    Start: int
    Turns: int
}

type QuadrantClutter = Map<Position, int>

type ShipCondition =
    | Operational
    | DeadInSpace
    | Destroyed

type SystemDamage =
    | Computer
    | WarpEngines
    | ShortRangeSensors
    | LongRangeSensors
    | Phasers
    | PhotonTubes
    | DamageControl
    | ShieldControl

type Klingon = {
    Sector: Position
    Energy: float
} with

    member this.ShipCondition =
        match this.Energy with
        | e when e < 0.0 -> ShipCondition.Destroyed
        | _ -> ShipCondition.Operational

type Damage = {
    Amount: int
    System: SystemDamage
} with
    member this.RepairSystem =
        let { Amount = x; System = s } = this

        match x with
        | (a) when a < 0 -> { Amount = min 0 (x + 1); System = s }
        | _ -> this

type Enterprise = {
    Sector: Position
    Quadrant: Position
    Energy: float
    Shields: float
    Torpedoes: int
    Damage: List<Damage>
}

type GameState = {
    Enterprise: Enterprise
    Klingons: Klingon []
    CurrentQuadrant: Sector [,]
    Quadrants: Quadrant [,]
    Stardate: Stardate
    QuadrantsScanned: Set<Position>
    Random: IRandomService
}

module GameDefaults =

    let initialDeviceStatus = [
        { Amount = 0; System = WarpEngines }
        { Amount = 0; System = ShortRangeSensors }
        { Amount = 0; System = LongRangeSensors }
        { Amount = 0; System = Phasers }
        { Amount = 0; System = PhotonTubes }
        { Amount = 0; System = DamageControl }
        { Amount = 0; System = ShieldControl }
        { Amount = 0; System = Computer }
    ]

    let deviceNames = [|
        "WARP ENGINES"
        "SHORT RANGE SENSORS"
        "LONG RANGE SENSORS"
        "PHASER CONTROL"
        "PHOTON TUBES"
        "DAMAGE CONTROL"
        "SHIELD CONTROL"
        "LIBRARY-COMPUTER"
    |]
