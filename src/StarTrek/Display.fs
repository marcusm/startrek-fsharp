module StarTrek.App.Display

open StarTrek
open StarTrek.Galaxy

let border = "-=--=--=--=--=--=--=--=-"

let statusLines (state: GameState) =
    [| sprintf "STARDATE  %g" state.Stardate
       sprintf "CONDITION %s" state.Condition
       sprintf "QUADRANT  %d,%d" state.Quadrant.X state.Quadrant.Y
       sprintf "SECTOR    %d,%d" state.Sector.X state.Sector.Y
       sprintf "ENERGY    %g" state.Energy
       sprintf "SHIELDS   %g" state.Shields
       sprintf "PHOTON TORPEDOES %d" state.Torpedoes
    |]

let renderSectorRow (sectorMap: Sector[,]) (row: int) =
    [| for col in 0 .. galaxySize - 1 -> sectorToChar sectorMap.[row, col] |]
    |> String.concat ""

let renderScanLines (state: GameState) =
    [| yield border
       for row in 0 .. galaxySize - 1 do
           yield renderSectorRow state.SectorMap row
       yield border |]
