module StarTrek.Galaxy

open StarTrek.GameTypes
open StarTrek.Enterprise

let galaxySize = 8

let createEmptySectorMap () : Sector[,] =
    Array2D.create galaxySize galaxySize Empty

let encodeQuadrant (quadrant: Quadrant) : int =
    quadrant.Klingons * 100 + quadrant.Starbases * 10 + quadrant.Stars

let decodeQuadrant (code: int) : Quadrant =
    { Quadrant = { X = 0; Y = 0 }
      Klingons = code / 100
      Starbases = (code % 100) / 10
      Stars = code % 10 }

let isValidPosition (pos: Position) =
    pos.X >= 1 && pos.X <= galaxySize && pos.Y >= 1 && pos.Y <= galaxySize

let sectorToChar (sector: Sector) =
    match sector with
    | Empty -> "   "
    | Star -> " * "
    | Klingon _ -> "+++"
    | Starbase -> ">!<"
    | Enterprise -> "<*>"

let courseDirections =
    [| (0.0, -1.0)   // Course 1: East (right)
       (-1.0, -1.0)  // Course 2: NE
       (-1.0, 0.0)   // Course 3: North (up)
       (-1.0, 1.0)   // Course 4: NW
       (0.0, 1.0)    // Course 5: West (left)
       (1.0, 1.0)    // Course 6: SW
       (1.0, 0.0)    // Course 7: South (down)
       (1.0, -1.0)   // Course 8: SE
    |]

let getCourseVector (course: float) =
    if course < 1.0 || course >= 9.0 then
        None
    else
        let idx = int (course - 1.0)
        let frac = course - float (int course)
        let (dy1, dx1) = courseDirections.[idx % 8]
        let (dy2, dx2) = courseDirections.[(idx + 1) % 8]
        let dx = dx1 + frac * (dx2 - dx1)
        let dy = dy1 + frac * (dy2 - dy1)
        Some (dx, dy)

let private generateKlingonCount (random: IRandomService) =
    let chance = random.NextDouble()
    match chance with
    | c when c > 0.98 -> 3
    | c when c > 0.95 -> 2
    | c when c > 0.80 -> 1
    | _ -> 0

let private generateStarbaseCount (random: IRandomService) =
    if random.NextDouble() < 0.04 then 1 else 0

let private generateStarCount (random: IRandomService) =
    random.Next(4, 7)

let private placeEnterprise (random: IRandomService) =
    let quadrant = { X = random.Next(1, 9); Y = random.Next(1, 9) }
    let sector = { X = random.Next(1, 9); Y = random.Next(1, 9) }
    resetEnterprise quadrant sector

let rec private tryGenerateGame (random: IRandomService) : GameState =
    let quadrants =
        Array2D.init galaxySize galaxySize (fun x y ->
            { Quadrant = { X = x + 1; Y = y + 1 }
              Klingons = generateKlingonCount random
              Starbases = generateStarbaseCount random
              Stars = generateStarCount random })

    let totalKlingons, totalStarbases =
        quadrants
        |> Seq.cast<Quadrant>
        |> Seq.fold (fun (k, b) q -> (k + q.Klingons, b + q.Starbases)) (0, 0)

    if totalKlingons < 12 || totalStarbases < 2 then
        tryGenerateGame random
    else
        let enterprise = placeEnterprise random

        let startDate = int (((random.NextDouble() * 20.0) + 20.0) * 100.0)
        let turns = int (random.NextDouble() * 20.0 + 20.0)

        { Enterprise = enterprise
          Klingons = [||]
          CurrentQuadrant = Array2D.zeroCreate galaxySize galaxySize
          Quadrants = quadrants
          Stardate =
            { Current = startDate
              Start = startDate
              Turns = turns } }

let initializeGame (seed: int) : GameState =
    let random = Random(seed) :> IRandomService
    tryGenerateGame random
