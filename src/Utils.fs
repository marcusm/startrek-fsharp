namespace StarTrek

module Utils =
    open GameTypes
    let getRandomSystem (random:IRandomService) = 
        randInst<SystemDamage> (random)

    let rec getRandomCoord (random: IRandomService) (exclude: QuadrantClutter) =
        let p =
            { x = random.Next(1, dim + 1)
              y = random.Next(1, dim + 1) }

        if Map.containsKey p exclude then
            getRandomCoord random exclude
        else
            p
