namespace StarTrek

module Utils =
    open GameTypes
    let getRandomSystem (random:IRandomService) = 
        randInst<SystemDamage> (random)

    let rec getRandomCoord (random: IRandomService) (exclude: QuadrantClutter) =
        let p =
            { X = random.Next(1, dim + 1)
              Y = random.Next(1, dim + 1) }

        if Map.containsKey p exclude then
            getRandomCoord random exclude
        else
            p
