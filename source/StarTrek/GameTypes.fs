namespace StarTrek

module GameTypes =

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
                match x with
                | (a) when a < 0 -> {amount = max 0 (x + 1) ; system = s}
                | _ -> this

    type Enterprise =
        { sector:Point;
        quadrant:Point;
        energy:float;
        shields:float;
        damage:List<Damage>}

