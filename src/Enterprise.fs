namespace StarTrek

module Enterprise =
    open GameTypes
    open Utils

    let condition enterprise =
        match enterprise.shields, enterprise.energy with
            | (s,e) when s < 0.0 -> ShipCondition.Destroyed
            | (s,e) when s > 0.0 && s < 1.1 && e = 0.0 -> ShipCondition.DeadInSpace
            | (_,_) -> ShipCondition.Operational

    let repairSystems enterprise =
        enterprise.damage 
            |> List.map (fun x -> x.repairSystem)
            |> List.filter (fun x -> x.amount <> 0)

    let updateDamageControl enterprise =
            let damage = repairSystems enterprise
            match System.Convert.ToDouble(System.Random().NextDouble()) with
            | (s) when s > 0.2 -> damage
            | _ ->
                let system = getRandomSystem
                let update = match System.Convert.ToDouble(System.Random().NextDouble()) with
                             | (c) when c >= 0.5 ->
                                System.Console.WriteLine("Repair")
                                {system=system; amount=System.Random().Next(1,5);}
                             | _ ->
                                System.Console.WriteLine("Damage")
                                {system=system; amount=System.Random().Next(1,5) * -1 }
                damage |> List.map (fun x ->
                            match x.system with
                            | r when r = system ->
                                let {amount=old} = x
                                {amount=old + update.amount; system=system}
                            | _ -> x)

    let tick enterprise =
            match condition enterprise with
            | ShipCondition.Destroyed -> enterprise
            | ShipCondition.DeadInSpace -> enterprise
            | ShipCondition.Operational -> { enterprise with damage=(updateDamageControl enterprise)}
