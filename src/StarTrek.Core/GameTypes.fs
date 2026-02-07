namespace StarTrek

type Position = { X: int; Y: int }

type Sector =
    | Empty
    | Star
    | Klingon of shieldPower: float
    | Starbase
    | Enterprise

type DeviceStatus = {
    WarpEngines: float
    ShortRangeSensors: float
    LongRangeSensors: float
    PhaserControl: float
    PhotonTubes: float
    DamageControl: float
    ShieldControl: float
    LibraryComputer: float
}

type QuadrantInfo = {
    Klingons: int
    Starbases: int
    Stars: int
}

type GameState = {
    Quadrant: Position
    Sector: Position
    Energy: float
    Shields: float
    Torpedoes: int
    Stardate: float
    TimeRemaining: float
    KlingonsRemaining: int
    BasesRemaining: int
    Devices: DeviceStatus
    QuadrantMap: QuadrantInfo[,]
    SectorMap: Sector[,]
    Condition: string
}

module GameDefaults =

    let initialDeviceStatus = {
        WarpEngines = 0.0
        ShortRangeSensors = 0.0
        LongRangeSensors = 0.0
        PhaserControl = 0.0
        PhotonTubes = 0.0
        DamageControl = 0.0
        ShieldControl = 0.0
        LibraryComputer = 0.0
    }

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
