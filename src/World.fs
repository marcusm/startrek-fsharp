namespace StarTrek

module World =
    open System
    open GameTypes
    open Utils

    let assignQuadrantKlingon chance =
        match chance with
        | (c) when c > 0.98 -> 3
        | (c) when c > 0.95 -> 2
        | (c) when c > 0.80 -> 1
        | _ -> 0

    let assignQuadrantBases chance =
        match chance with
        | (c) when c > 0.96 -> 1
        | _ -> 0

    let clutterMap (random: IRandomService) (e: Point) (klingons: int) (bases: int) (stars: int) =
        let rec addCluter currentMap i count v =
            if i >= count then
                currentMap
            else
                let newMap =
                    Map.add (getRandomCoord random currentMap) v currentMap

                addCluter newMap (i + 1) v count

        let clutter = Map.add e enterpriseId Map.empty
        let clutter = addCluter clutter 0 klingons klingonId
        let clutter = addCluter clutter 0 bases baseId
        addCluter clutter 0 stars starId

    let quadrantTemplate (clutter: QuadrantClutter) (x: int) (y: int) =
        let p = { X = x; Y = y }

        match (Map.containsKey p clutter) with
        | true -> clutter.[p]
        | _ -> 0

    let createQuadrant (random: IRandomService) x y =
        let bases =
            assignQuadrantBases (random.NextDouble())

        let klingons =
            assignQuadrantKlingon (random.NextDouble())

        { Quadrant = { X = x; Y = y }
          Klingons = klingons
          Bases = bases
          Stars = 5 }

    let placeQuadrant (random: IRandomService) (clutter: QuadrantClutter) sector =
        let quadrantGen = quadrantTemplate clutter
        let current = Array2D.initBased 1 1 8 8 quadrantGen

        current

    let resetEnterprise enterprise =
        { enterprise with
              Torpedoes = 10
              Energy = 3000.0
              Shields = 0.0
              Damage = [] }

    let createEnterprise (random: IRandomService) =
        { Sector =
              { X = random.Next(1, dim + 1)
                Y = random.Next(1, dim + 1) }
          Quadrant =
              { X = random.Next(1, dim + 1)
                Y = random.Next(1, dim + 1) }
          Energy = 3000.0
          Shields = 0.0
          Torpedoes = 10
          Damage = [] }

    let setQuadrantKlingons (clutter: QuadrantClutter) =
        let filter (c: Point * int) =
            let p, v = c
            v = klingonId

        let transform (c: Point * int) =
            let p, v = c

            { Sector = { X = p.X; Y = p.Y }
              Energy = 200.0 }

        let klingons =
            Map.toSeq clutter
            |> Seq.filter filter
            |> Seq.map transform
            |> Seq.toArray

        klingons

    let enterQuadrant (random: IRandomService) (state: GameState) (e: Enterprise) =
        let quadrant =
            Array2D.get state.Quadrants e.Quadrant.X e.Quadrant.Y

        let clutter =
            clutterMap random e.Sector quadrant.Klingons quadrant.Bases quadrant.Stars

        let current = placeQuadrant random clutter e.Sector
        let klingons = setQuadrantKlingons clutter

        { state with
              Klingons = klingons
              CurrentQuadrant = current }


    let startNewGame (random: IRandomService) =
        let quadrants =
            Array2D.initBased 1 1 dim dim (fun x y -> createQuadrant random x y)

        let enterprise = createEnterprise random

        let startDate =
            int (((random.NextDouble()) * 20.0 + 20.0) * 100.0)

        let state =
            { Enterprise = enterprise
              Klingons = Array.empty
              CurrentQuadrant = (Array2D.zeroCreateBased 1 1 dim dim)
              Quadrants = quadrants
              Stardate =
                  { Current = startDate
                    Start = startDate
                    Turns = 30 } }

        enterQuadrant random state enterprise
