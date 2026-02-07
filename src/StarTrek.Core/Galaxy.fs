module StarTrek.Galaxy

open StarTrek

let galaxySize = 8

let createEmptySectorMap () : Sector[,] =
    Array2D.create galaxySize galaxySize Empty

let encodeQuadrant (info: QuadrantInfo) : int =
    info.Klingons * 100 + info.Starbases * 10 + info.Stars

let decodeQuadrant (code: int) : QuadrantInfo =
    { Klingons = code / 100
      Starbases = (code % 100) / 10
      Stars = code % 10 }

let isValidPosition (pos: Position) =
    pos.X >= 0 && pos.X < galaxySize && pos.Y >= 0 && pos.Y < galaxySize

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
