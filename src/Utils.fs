namespace StarTrek

module Utils =
    open GameTypes
    let getRandomSystem = randInst<SystemDamage>()
    
    let rec getRandomCoord (random:System.Random) (exclude:QuadrantClutter) =
        let p = {x=random.Next(1,dim+1);y=random.Next(1,dim+1)};
        if Map.containsKey p exclude then
            getRandomCoord random exclude
        else
            p



