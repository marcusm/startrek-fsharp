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

    let createQuadrant (random: System.Random) x y =
        let bases = assignQuadrantBases (random.NextDouble())
        let klingons = assignQuadrantKlingon (random.NextDouble())
        {quadrant={x=x;y=y};klingons=klingons;bases=bases;stars=5;}

    let placeQuadrantItem (random:System.Random) (quadrant:int[,]) count id =
        let mutable placedItem = 0
        while placedItem < count do
            let x = random.Next(1, dim+1)
            let y = random.Next(1, dim+1)

            if quadrant.[x,y] = 0 then
                quadrant.[x,y] <- id 
                placedItem<- placedItem + 1

    let placeQuadrant (random:System.Random) (quadrant: Quadrant) sector =
        let current = Array2D.zeroCreateBased 1 1 dim dim
        current.[sector.x,sector.y] <- enterpriseId
        placeQuadrantItem random current quadrant.klingons klingonId
        placeQuadrantItem random current quadrant.bases baseId
        placeQuadrantItem random current quadrant.stars starId

        current

    let resetEnterprise enterprise =
        { enterprise with torpedoes = 10; energy = 3000.0; shields = 0.0; damage = []; }

    let createEnterprise (random:System.Random) =
        {sector = {x=random.Next(1,dim+1);y=random.Next(1,dim+1)};
         quadrant = {x=random.Next(1,dim+1);y=random.Next(1,dim+1)};
         energy = 3000.0;
         shields = 0.0;
         torpedoes = 10;
         damage = []}

    let placeQuadrantKlingons (random:System.Random) count =
        match count with
        | 0 -> Array.empty<Klingon>
        | _ ->
            Array.init count (fun _ ->
                {Klingon.sector = {x=random.Next(1,dim+1);y=random.Next(1,dim+1);}; energy = 200.0;})

    let startNewGame (random : System.Random) =
        let quadrants = Array2D.initBased 1 1 dim dim (fun x y -> createQuadrant random x y)
        let enterprise = createEnterprise random
        let current = placeQuadrant random quadrants.[enterprise.quadrant.x,enterprise.quadrant.y] enterprise.sector

        let quad = quadrants.[enterprise.quadrant.x,enterprise.quadrant.y] 
        let startDate = int(((random.NextDouble()) * 20.0 + 20.0) * 100.0)
        { klingons=(placeQuadrantKlingons random quad.klingons);
          currentQuadrant=current;
          quadrants=quadrants;
          stardate={current=startDate;start=startDate;turns=30;}}
