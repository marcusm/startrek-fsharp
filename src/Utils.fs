namespace StarTrek

module Utils =
    open GameTypes

    let getRandomSystem (random: IRandomService) = randInst<SystemDamage> (random)

    let rec getRandomCoord (random: IRandomService) (exclude: QuadrantClutter) =
        let p =
            { X = random.Next(1, dim + 1)
              Y = random.Next(1, dim + 1) }

        if Map.containsKey p exclude then
            getRandomCoord random exclude
        else
            p

    let inline euclidean (a: Point) (b: Point) =
        pown (a.X - b.X) 2 + pown (a.Y - b.Y) 2
        |> float
        |> sqrt

    let inline euclideanSquared (a: Point) (b: Point) =
        pown (a.X - b.X) 2 + pown (a.Y - b.Y) 2 |> float

    let nearbySpace (p: Point) =
        seq {
            for x in (max 1 (p.X - 1)) .. (min (dim + 1) (p.X + 1)) do
                for y in (max 1 (p.Y - 1)) .. (min (dim + 1) (p.Y + 1)) do
                    let n = { X = x; Y = y }
                    if p <> n then yield n
        }
