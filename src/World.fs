namespace StarTrek

module World =
    open System
    open GameTypes

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

    let rec getRandomCoord (random:System.Random) (exclude:QuadrantClutter) =
        let p = {x=random.Next(1,dim+1);y=random.Next(1,dim+1)};
        if Map.containsKey p exclude then
            getRandomCoord random exclude
        else
            p

    let clutterMap (random:Random) (e:Point) (klingons:int) (bases:int) (stars:int) =
        let rec addCluter currentMap i count v =
            if i >= count then currentMap
            else
                let newMap = Map.add (getRandomCoord random currentMap) v currentMap
                addCluter newMap (i+1) v count
    
        let clutter = Map.add e enterpriseId Map.empty
        let clutter = addCluter clutter 0 klingons klingonId
        let clutter = addCluter clutter 0 bases baseId
        addCluter clutter 0 stars starId

    let quadrantTemplate (clutter:QuadrantClutter) (x:int) (y:int) =
        let p = {x=x;y=y}
        match (Map.containsKey p clutter) with
        | true -> clutter.[p]
        | _ -> 0

    let createQuadrant (random: System.Random) x y =
        let bases = assignQuadrantBases (random.NextDouble())
        let klingons = assignQuadrantKlingon (random.NextDouble())
        {quadrant={x=x;y=y};klingons=klingons;bases=bases;stars=5;}

    let placeQuadrant (random:System.Random) (clutter:QuadrantClutter) sector =
        let quadrantGen = quadrantTemplate clutter
        let current = Array2D.initBased 1 1 8 8 quadrantGen

        current 

    let resetEnterprise enterprise =
        { enterprise with torpedoes = 10; energy = 3000.0; shields = 0.0; damage = []; }

    let createEnterprise (random:System.Random) =
        {sector = {x=random.Next(1,dim+1);y=random.Next(1,dim+1)};
         quadrant = {x=random.Next(1,dim+1);y=random.Next(1,dim+1)};
         energy = 3000.0;
         shields = 0.0;
         torpedoes = 10
         damage = []}

    let setQuadrantKlingons (clutter:QuadrantClutter) =
        let filter (c:Point*int) =
            let p,v = c
            v = klingonId

        let transform (c:Point*int) =
            let p,v = c
            { sector = { x=p.x;y=p.y;}; energy=200.0 }

        let klingons =
            Map.toSeq clutter
            |> Seq.filter filter
            |> Seq.map transform
            |> Seq.toArray

        klingons

    let enterQuadrant (random:Random) (state:GameState) (e:Enterprise) =
        let quadrant = Array2D.get state.quadrants e.quadrant.x e.quadrant.y
        let clutter = clutterMap random e.sector quadrant.klingons quadrant.bases quadrant.stars
        let current = placeQuadrant random clutter e.sector
        let klingons = setQuadrantKlingons clutter
        { state with klingons=klingons; currentQuadrant = current }


    let startNewGame (random : System.Random) =
        let quadrants = Array2D.initBased 1 1 dim dim (fun x y -> createQuadrant random x y)
        let enterprise = createEnterprise random
        let startDate = int(((random.NextDouble()) * 20.0 + 20.0) * 100.0)
        let state = { klingons = Array.empty; 
                    currentQuadrant = (Array2D.zeroCreateBased 1 1 dim dim);
                    quadrants=quadrants;
                    stardate={current=startDate;start=startDate;turns=30};}
        
        enterQuadrant random state enterprise
