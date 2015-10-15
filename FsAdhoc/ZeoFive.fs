namespace ZeoFive

// 目標
// ほとんどの種類のデッキを自動生成して、自動で戦わせ、どのようなデッキが一番強いか統計的に調べる
// また、それにより現実的な思考ルーチンを作成する。

open System
open System.Collections.Generic
open Uedai.Utilities

module Collections =
    module Seq =
        let cycle (s : #seq<'T>) =
            let iter = s.GetEnumerator()
            seq {
                while true do
                    while iter.MoveNext() do
                        yield iter.Current
                    iter.Reset()
            }

    module List =
        let maxWith comparer = function
            | [] -> invalidArg "list" "no elements"
            | x :: xs ->
                List.fold (fun maximam elem ->
                    if comparer maximam elem < 0 then elem else maximam
                ) x xs
        let minWith comparer = List.rev >> maxWith (fun a b -> - (comparer a b))
    (*
        /// [0, n) のすべての順列からなる列
        let rec permutations : (int -> int list list) =
            let f = function
                | 0 -> []
                | n when n >= 0 ->
                    let smallPerms = permutations (n - 1)
                    let src = Seq.iota n |> List.ofSeq
                    [ for i in src do
                            let src' = src |> List.filter ((<>) i)
                            for perm in smallPerms do
                                yield i :: (src' |> List.permute (fun i -> List.nth perm i)) ]
                | _ -> invalidArg "n" "must be natural."
            Others.memorize f
    //*)
            
    module Tuple =
        let map2 f (x0, x1) =
            (f x0, f x1)
        let iter2 f (x0, x1) =
            f x0; f x1; ()
        let zip2 (x0, x1) (y0, y1) =
            ((x0, y0), (x1, y1))
        let rev2 (x0, x1) = (x1, x0)

    (*
    type ComparisonValue = | Less | Equal | Greater
    let (|LessEq|Greater|) = function
        | Less | Equal -> LessEq
        | Greater -> Greater
    let (|Less|GreaterEq|) = function
        | Less -> Less
        | Equal | Greater -> GreaterEq
    //*)

open Collections

module ZeoFive =
    let rnd = new Random()
    let (|Deref|) (r : 'T ref) = !r
    type WayToAttack =
        | Physic | Magic
    with
        static member AllCase = [ Physic; Magic ]
        static member Inv = function
            | Physic -> Magic
            | Magic -> Physic

    type CardSpec = {
        Name : string
        Life : int
        Speed : int
        Atk : int
        Int : int
    }

    /// length must be 5.
    type DeckList = CardSpec list
    
    type BfState = { CurLife : int; NextAttack : WayToAttack option }
    type CardState =
        | InHand
        | OnBattlefield of BfState
        | Dead

    type Card = {
        Spec : CardSpec
        State : CardState
        Owner : Player
    }
    with
        static member instanciate pl card =
            { Spec = card; State = CardState.InHand; Owner = pl }
            
        static member damageValueBy way { Spec = card } =
            match way with
            | Physic -> card.Atk
            | Magic -> card.Int

        /// 損傷率
        static member damageRate = function
            | { Spec = card; State = OnBattlefield { CurLife = curLife } } ->
                (float curLife) / (float card.Life)
            | _ -> 0.0

        /// 戦場に出ていて最も健康な状態
        static member maxwell (card : Card) =
            { card with State = OnBattlefield { CurLife = card.Spec.Life; NextAttack = None } }

        static member decideWayToAttack way (card : Card) =
            match card.State with
            | OnBattlefield st ->
                { card with State = OnBattlefield { st with NextAttack = Some way }}
            | _ -> failwith "debug it"

        static member dealtDamage damageValue (card : Card) =
            match card.State with
            | OnBattlefield ({ CurLife = life } as st) ->
                let remainLife = (* max 0 *) (life - damageValue)

                if remainLife > 0
                    then { card with State = OnBattlefield { st with CurLife = remainLife } }
                    else { card with State = Dead }
            | _ -> failwith "debug it"
            
        static member dealtDamageFrom attacker defender =
            match attacker.State with
            | CardState.OnBattlefield { NextAttack = Some way } ->
                let damageValue = (attacker |> Card.damageValueBy way)
                defender |> Card.dealtDamage damageValue

            | CardState.Dead -> defender
            | _ -> failwith "debug it"
            
    /// who think how to play the game
    and Commander =
        abstract member ChooseCard : Game * Player -> Card ref
        abstract member ChooseWayToAttack : Game * Player -> WayToAttack
    
    /// who is a character in game, has his/her card enter battlefield and attack the opponent's one.
    and Player (commander_ : Commander, deck : DeckList) =
        member this.BringEntities =
            deck |> List.map (Card.instanciate this >> ref)
        
        member private this.Commander = commander_
        static member commander (pl : Player) = pl.Commander

        static member isDead (game : Game) pl =
            pl |> Game.ownCards game |> List.forall (fun (Deref c) -> c.State = Dead)

    and Game (firstPlayer : Player, secondPlayer : Player) as this =
        //let turns = [firstPlayer; secondPlayer] |> Seq.cycle
        
        let entities_ = lazy [
            let ent1, ent2 = (this.Players |> Tuple.map2 (fun (pl : Player) -> pl.BringEntities))
            yield! ent1
            yield! ent2
        ]

        member private this.Entities = entities_.Value
        static member private entities (this : Game) = this.Entities
        
        member this.Players = (firstPlayer, secondPlayer)
        //member this.PlayerList = [ firstPlayer; secondPlayer ]

        /// areas
        static member opponent (game : Game) pl =
            if   pl = (game.Players |> fst) then (game.Players |> snd)
            elif pl = (game.Players |> snd) then (game.Players |> fst)
            else invalidArg "pl" "he/she is not playing this game"

        static member ownCards game pl =
            game |> Game.entities |> List.filter (fun (Deref c) -> c.Owner = pl)

        static member hand game pl =
            game |> Game.entities |> List.filter (fun (Deref c) -> c.State = CardState.InHand && c.Owner = pl)
            
        static member graveyard game pl =
            game |> Game.entities |> List.filter (fun (Deref c) -> c.State = CardState.Dead && c.Owner = pl)
            
        static member battlefield (this : Game) =
            this.Players |> Tuple.map2 (Game.activeChar this)
            
        static member activeChar game pl =
            game |> Game.entities |> List.tryPick (fun ((Deref c) as r) ->
                match c with
                | { State = OnBattlefield _; Owner = pl' } when pl' = pl -> Some r
                | _ -> None
            )
        
        /// decisions
        static member private castCard pl (this : Game) =
            if (Game.activeChar this pl).IsNone then
                let r =
                    (pl |> Player.commander).ChooseCard(this, pl)
                
                if (!r).State = CardState.InHand && (!r).Owner = pl then
                    Some r
                else
                    raise (new GameEndException (GameResult.Win (Game.opponent this pl)))
            else None

        static member private declareToAttack pl (this : Game) =
            match Game.activeChar this pl with
            | Some ((Deref { State = OnBattlefield st }) as r) ->
                let way =
                    match st.NextAttack with
                    | None ->
                        (pl |> Player.commander).ChooseWayToAttack(this, pl)
                    | Some way ->
                        way |> WayToAttack.Inv
                r := (!r) |> Card.decideWayToAttack way
            | _ -> failwith "debug it"
            
        /// game flows
        member this.Begin() =
            this.MainLoop()

        member this.End result = result
            
        member private this.MainLoop() =
            this.CastingPhase()
            this.DeclaringPhase()
            this.CombatPhase()
            match this.CleanUp() with
            | Some result -> this.End result
            | None -> this.MainLoop()
            
        member private this.CastingPhase() =
            this.Players
                |> Tuple.map2 (fun pl -> this |> Game.castCard pl)
                |> Tuple.iter2 (function 
                    | Some r -> r := (!r) |> Card.maxwell
                    | None -> ())
            
        member private this.DeclaringPhase() =
            this.Players |> Tuple.iter2 (fun pl -> this |> Game.declareToAttack pl)
            
        static member CompareSpeed c1 c2 =
            let spd1, spd2 = (c1, c2) |> Tuple.map2 (fun c -> c.Spec.Speed)
            compare spd1 spd2

        member private this.CombatPhase() =
            let c1, c2 =
                match this |> Game.battlefield with
                | (Some f, Some s) -> f, s
                | _ -> failwith "battlefield must be full"
                
            match Game.CompareSpeed (!c1) (!c2) with
            | 0 ->
                (*  速さが同じなら同時に与え合う
                    (HSP版では損傷率の低い方が若干有利のランダムだった)
                    let r1, r2 = (c1, c2) |> Tuple.map2 (Card.damageRate)
                //*)
                let AfterCombatCard1 = (!c1) |> Card.dealtDamageFrom (!c2)
                let AfterCombatCard2 = (!c2) |> Card.dealtDamageFrom (!c1)
                c1 := AfterCombatCard1
                c2 := AfterCombatCard2
            | cmp ->
                let combatOrder =
                    [ (c1, c2); (c2, c1) ]
                    |> (fun ls -> if cmp < 0 then ls |> List.rev else ls)

                for (attacker, defender) in combatOrder do
                    defender := (!defender) |> Card.dealtDamageFrom (!attacker)
            ()

        member private this.CleanUp() =
            match this.Players |> Tuple.map2 (Player.isDead this) with
            | true, true  -> Some (GameResult.Tie)
            | true, false -> Some (GameResult.Win (this.Players |> fst))
            | false, true -> Some (GameResult.Win (this.Players |> snd))
            | false, false -> None

    and GameResult =
        | Tie
        | Win of Player

    and GameEndException (r : GameResult) =
        inherit System.Exception()

    /// AI
    module AICommander =
        module CombatSimulation =
            type ResultValue =
                | YourWin of Card
                | OppoWin of Card
                | BothDie

            type Result =
                | SomeCommand of (WayToAttack * ResultValue) list
                | NoneCommand of ResultValue

            let betterResult r1 r2 =
                match r1, r2 with
                | YourWin c1, YourWin c2 ->
                    match c1.State, c2.State with
                    | OnBattlefield { CurLife = l1; NextAttack = Some w1 },
                        OnBattlefield { CurLife = l2; NextAttack = Some w2 } ->
                        let d1, d2 =
                            (w1, w2)
                            |> Tuple.zip2 (c1, c2)
                            |> Tuple.map2 (fun (c, w) -> c |> Card.damageValueBy w)
                        compare (l1, d1) (l2, d2)
                    | _ -> failwith "debug it"

                | OppoWin c1, OppoWin c2 ->
                    match c1.State, c2.State with
                    | OnBattlefield { CurLife = l1 },
                        OnBattlefield { CurLife = l2 } ->
                        compare l1 l2
                    | _ -> failwith "debug it"

                | BothDie, BothDie -> 0

                | BothDie, YourWin _
                | OppoWin _, YourWin _
                | OppoWin _, BothDie  -> -1

                | YourWin _, OppoWin _
                | YourWin _, BothDie
                | BothDie, OppoWin _ -> 1

        /// 戦闘シミュレーション
        (*
            相手に選択肢があるなら、相手にとって有利な方を選ばれることにする。
            両者の Atk, Int のうちいずれかは必ず0より大きいので有限回で終わる。
            戦闘後の生存した側の状態についても考慮すべき。
            つまり、自分が勝つ場合は、HPが多く、次に良い技を撃てる方がよい。
            逆に、相手が勝つ場合は、HPが少ない方がよい。

            攻撃方法を反転させるのを忘れている
        //*)
        let combatSimulation =

            let rec g (c1, c2) =
                let c1', c2' =
                    (c1 |> Card.dealtDamageFrom c2),
                    (c2 |> Card.dealtDamageFrom c1)

                match c1'.State, c2'.State, Game.CompareSpeed c1 c2 with
                | _, CardState.Dead, Integer.Positive _ -> CombatSimulation.YourWin c1
                | CardState.Dead, _, Integer.Negative _ -> CombatSimulation.OppoWin c2
                | CardState.Dead, CardState.Dead, _ -> CombatSimulation.BothDie
                | CardState.Dead, _, Integer.Zero
                | CardState.Dead, _, Integer.Positive _ -> CombatSimulation.OppoWin c2'
                | _, CardState.Dead, Integer.Zero
                | _, CardState.Dead, Integer.Negative _ -> CombatSimulation.YourWin c1'
                | _ -> g (c1', c2')

            let oppoCommand (c1, c2) =
                match c2.State with
                | OnBattlefield { NextAttack = None } ->
                    [ for way in WayToAttack.AllCase do
                        yield g (c1, (c2 |> Card.decideWayToAttack way)) ]
                    |> List.minWith CombatSimulation.betterResult

                | OnBattlefield { NextAttack = Some _ } ->
                    g (c1, c2)
                | _ -> failwith "debug it"
                
            let yourCommand (c1, c2) =
                match c1.State with
                | OnBattlefield { NextAttack = None } ->
                    [ for cmd in WayToAttack.AllCase do
                        yield (cmd, oppoCommand ((c1 |> Card.decideWayToAttack cmd), c2)) ]
                    |> CombatSimulation.SomeCommand
                | OnBattlefield { NextAttack = Some _ } ->
                    CombatSimulation.NoneCommand (oppoCommand (c1, c2))
                | _ -> failwith "debug it"

            yourCommand

        let chooseCardAtRandom (game, pl) =
            let hand = Game.hand game pl
            List.nth hand (rnd.Next (hand |> List.length))

        let opponentsActiveChar (game, pl) =
            Game.activeChar game (Game.opponent game pl)

        /// ランダムに意思決定する司令官
        type RandomCommander() =
            interface Commander with
                member this.ChooseCard (game, pl) =
                    chooseCardAtRandom (game, pl)
                member this.ChooseWayToAttack (game, pl) =
                    if rnd.Next(2) = 0 then Physic else Magic

        // 一手先まで考える司令官
        type NormalCommander() =
            let mutable nextAttack = None

            // 場に出ている相手のカードを打ち負かせるか否か
            let defeatable (game, pl) yourCard oppoCard =
                combatSimulation (yourCard |> Card.maxwell, oppoCard)

            let simulate candidates oppoCardRef =
                [ for yourCharRef in candidates do
                    let yourChar = !yourCharRef |> Card.maxwell
                    match combatSimulation (yourChar, !oppoCardRef) with
                    | CombatSimulation.SomeCommand maps ->
                        for (way, result) in maps do
                            yield (yourCharRef, Some way), result
                    | CombatSimulation.NoneCommand r ->
                        yield (yourCharRef, None), r ]
                |> List.maxWith (fun (_, r1) (_, r2) -> CombatSimulation.betterResult r1 r2)
                |> fst
                

            interface Commander with
                // CombatSimulation の思考ルーチンに従って選択する
                // ゲームの最初、および相打ちした後にはランダムに選ぶ
                member this.ChooseCard (game, pl) =
                    match opponentsActiveChar (game, pl) with
                    | None ->
                        chooseCardAtRandom (game, pl)
                    | Some oppoChar ->
                        let hand = Game.hand game pl
                        let (yourCharRef, way) = simulate hand oppoChar

                        nextAttack <- way
                        yourCharRef

                member this.ChooseWayToAttack (game, pl) =
                    match nextAttack with
                    | Some way -> way
                    | None ->
                        match (Game.activeChar game pl), (opponentsActiveChar (game, pl)) with
                        | Some yourChar, Some oppoChar ->
                            let (_, way) = simulate [yourChar] oppoChar
                            nextAttack <- way
                            assert (nextAttack.IsSome)
                            nextAttack |> Option.get
                        | _ -> failwith "debug it"
                        
module Sample =
    open ZeoFive

    let card name life spd atk int =
        { Name = name; Life = life; Speed = spd; Atk = atk; Int = int }
    let ``サンプルデッキ0`` =
        [
            card "ハンター" 10 140 70 20
            card "ミッダー" 110 0 70 20
            card "エリマー" 10 30 160 0
            card "ヘルサー" 150 0 30 20
            card "バランサー" 80 40 50 30
        ]

    let ``サンプルデッキ1`` =
        [
            card "掠奪のガニシ" 65 55 10 70
            card "沈黙のシアン" 10 80 100 10
            card "封印のルベノ" 10 40 150 0
            card "哀愁のノーバ" 5 135 50 10
            card "失速のバロガ" 130 0 30 40
        ]
        
    let player deck = new Player(new AICommander.NormalCommander(), deck)

    let sample() =

        let pl1 = player ``サンプルデッキ0``
        let pl2 = player ``サンプルデッキ1``
        let game = new Game(pl1, pl2)
        let r = game.Begin()
        r
