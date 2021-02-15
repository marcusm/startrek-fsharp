namespace StarTrek

module GameTypes =
    let dim = 8

    let enterpriseId = 1
    let baseId       = 2
    let klingonId    = 3
    let starId       = 4

    type Point = {x:int; y:int;}

    type Quadrant = {quadrant:Point;bases:int;klingons:int;stars:int}

    type Stardate = {current:int;start:int;turns:int;}

    type ShipCondition =
        | Operational
        | DeadInSpace
        | Destroyed

    type Klingon =
        {sector:Point; energy: float;}

        member this.ShipCondition =
            match this.energy with
            | e when e < 0.0 -> ShipCondition.Destroyed
            | _ -> ShipCondition.Operational

    type GameState  = {klingons:Klingon[];currentQuadrant:int[,];quadrants:Quadrant[,];stardate:Stardate;}

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
        {amount:int;system:SystemDamage} with
            member this.repairSystem =
                let {amount=x; system=s} = this
                match x with
                | (a) when a < 0 -> {amount = min 0 (x + 1) ; system = s}
                | _ -> this

    type Enterprise =
        { sector:Point;
        quadrant:Point;
        energy:float;
        shields:float;
        torpedoes:int;
        damage:List<Damage>}

