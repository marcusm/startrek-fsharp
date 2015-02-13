module GameTypes

type Point = {x:int; y:int;}


type ShipCondition =
    | Operational
    | DeadInSpace
    | Destroyed

type Klingon = 
    {location:Point; energy: int;}

    member this.ShipCondition =
        match this.energy with
        | e when e < 0 -> ShipCondition.Destroyed  
        | _ -> ShipCondition.Operational  

type SystemDamage =
  | Computer
  | WarpEngines
  | ShortRangeSensors
  | LongRangeSensors
  | Phasers
  | PhotonTubes
  | DamageControl
  | ShieldControl

let randInst<'T>() = 
  let cases = Reflection.FSharpType.GetUnionCases(typeof<'T>)
  let index = System.Random().Next(cases.Length)
  let case = cases.[index]
  Reflection.FSharpValue.MakeUnion(case, [||]) :?> 'T

type Damage = 
    {amount:int;system:SystemDamage;}
        member this.repairSystem =
            let {amount=x; system=s} = this
            {amount = max 0 (x + 1) ; system = s}

type Enterprise = 
    { sector:Point;
    quadrant:Point;
    energy:int;
    shields:double;
    isDocked:bool;
    damage:List<Damage>}

    member this.getRandomSystem = randInst<SystemDamage>()
        
    member this.Condition =
        match this.shields, this.energy with
            | (s,e) when s < 0.0 -> ShipCondition.Destroyed
            | (s,e) when s > 0.0 && s < 1.1 && e = 0 -> ShipCondition.DeadInSpace
            | (_,_) -> ShipCondition.Operational

    member private this.repairSystems =
        this.damage |> List.map (fun x -> x.repairSystem)

    member private this.updateDamageControl =
        let damage = this.repairSystems
        match System.Convert.ToDouble(System.Random().NextDouble) with
        | (s) when s > 0.2 -> damage
        | _ ->
            let system = this.getRandomSystem 
            let update = match System.Convert.ToDouble(System.Random().NextDouble) with
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

    member this.Tick =
        match this.Condition with
        | ShipCondition.Destroyed -> this
        | ShipCondition.DeadInSpace -> this
        | ShipCondition.Operational -> { this with damage=this.updateDamageControl}
