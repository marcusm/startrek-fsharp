namespace StarTrek

module Enterprise =
    open GameTypes
    open Utils

    let condition enterprise =
        match enterprise.Shields, enterprise.Energy with
        | (s, e) when s < 0.0 -> ShipCondition.Destroyed
        | (s, e) when s > 0.0 && s < 1.1 && e = 0.0 -> ShipCondition.DeadInSpace
        | (_, _) -> ShipCondition.Operational

    let repairSystems enterprise =
        enterprise.Damage
        |> List.map (fun x -> x.RepairSystem)
        |> List.filter (fun x -> x.Amount <> 0)

    let updateDamageControl (random: IRandomService) (enterprise: Enterprise) =
        let damage = repairSystems enterprise

        match System.Convert.ToDouble(random.NextDouble()) with
        | (s) when s > 0.2 -> damage
        | _ ->
            let system = getRandomSystem random

            let update =
                match System.Convert.ToDouble(random.NextDouble()) with
                | (c) when c >= 0.5 ->
                    System.Console.WriteLine("Repair")

                    { System = system
                      Amount = System.Random().Next(1, 5) }
                | _ ->
                    System.Console.WriteLine("Damage")

                    { System = system
                      Amount = System.Random().Next(1, 5) * -1 }

            damage
            |> List.map
                (fun x ->
                    match x.System with
                    | r when r = system ->
                        let { Amount = old } = x

                        { Amount = old + update.Amount
                          System = system }
                    | _ -> x)

    let tick (random: IRandomService) (enterprise: Enterprise) =
        match condition enterprise with
        | ShipCondition.Destroyed -> enterprise
        | ShipCondition.DeadInSpace -> enterprise
        | ShipCondition.Operational ->
            { enterprise with
                  Damage = (updateDamageControl random enterprise) }
