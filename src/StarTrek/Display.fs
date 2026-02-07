module StarTrek.App.Display

open StarTrek.GameTypes
open StarTrek.Galaxy

let border = "-=--=--=--=--=--=--=--=-"

let statusLines (state: GameState) =
    [| sprintf "STARDATE  %d" state.Stardate.Current
       sprintf "QUADRANT  %d,%d" state.Enterprise.Quadrant.X state.Enterprise.Quadrant.Y
       sprintf "SECTOR    %d,%d" state.Enterprise.Sector.X state.Enterprise.Sector.Y
       sprintf "ENERGY    %g" state.Enterprise.Energy
       sprintf "SHIELDS   %g" state.Enterprise.Shields
       sprintf "PHOTON TORPEDOES %d" state.Enterprise.Torpedoes
    |]

let renderSectorRow (sectorMap: Sector[,]) (row: int) =
    [| for col in 0 .. galaxySize - 1 -> sectorToChar sectorMap.[row, col] |]
    |> String.concat ""

let renderScanLines (state: GameState) =
    [| yield border
       for row in 0 .. galaxySize - 1 do
           yield renderSectorRow state.CurrentQuadrant row
       yield border |]
