module StarTrek.App.Commands

open StarTrek

let warpEngineControl (state: GameState) =
    printfn "WARP ENGINE CONTROL -- NOT YET IMPLEMENTED"
    state

let shortRangeScan (state: GameState) =
    printfn "SHORT RANGE SENSOR SCAN -- NOT YET IMPLEMENTED"
    state

let longRangeScan (state: GameState) =
    printfn "LONG RANGE SENSOR SCAN -- NOT YET IMPLEMENTED"
    state

let phaserControl (state: GameState) =
    printfn "PHASER CONTROL -- NOT YET IMPLEMENTED"
    state

let photonTorpedoControl (state: GameState) =
    printfn "PHOTON TORPEDO CONTROL -- NOT YET IMPLEMENTED"
    state

let shieldControl (state: GameState) =
    printfn "SHIELD CONTROL -- NOT YET IMPLEMENTED"
    state

let damageControlReport (state: GameState) =
    printfn "DAMAGE CONTROL REPORT -- NOT YET IMPLEMENTED"
    state

let libraryComputer (state: GameState) =
    printfn "LIBRARY COMPUTER -- NOT YET IMPLEMENTED"
    state

let help () =
    let path = System.IO.Path.Combine("doc", "help.txt")
    if System.IO.File.Exists(path) then
        System.IO.File.ReadAllText(path) |> printfn "%s"
    else
        printfn "HELP FILE NOT FOUND"
