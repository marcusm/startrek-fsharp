module StarTrek.App.Commands

open StarTrek.GameTypes

let warpEngineControl (state: GameState) =
    ["WARP ENGINE CONTROL -- NOT YET IMPLEMENTED"], state

let shortRangeScan (state: GameState) =
    ["SHORT RANGE SENSOR SCAN -- NOT YET IMPLEMENTED"], state

let longRangeScan (state: GameState) =
    ["LONG RANGE SENSOR SCAN -- NOT YET IMPLEMENTED"], state

let phaserControl (state: GameState) =
    ["PHASER CONTROL -- NOT YET IMPLEMENTED"], state

let photonTorpedoControl (state: GameState) =
    ["PHOTON TORPEDO CONTROL -- NOT YET IMPLEMENTED"], state

let shieldControl (state: GameState) =
    ["SHIELD CONTROL -- NOT YET IMPLEMENTED"], state

let damageControlReport (state: GameState) =
    ["DAMAGE CONTROL REPORT -- NOT YET IMPLEMENTED"], state

let libraryComputer (state: GameState) =
    ["LIBRARY COMPUTER -- NOT YET IMPLEMENTED"], state

let help () : string list =
    let path = System.IO.Path.Combine("doc", "help.txt")
    if System.IO.File.Exists(path) then
        System.IO.File.ReadAllLines(path) |> Array.toList
    else
        ["HELP FILE NOT FOUND"]
