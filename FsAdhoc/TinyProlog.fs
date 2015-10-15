namespace Uedai

module TinyProlog =
    type TinyPrologBuilder() =
        member this.Zero() = []
        member this.Delay f = f
        member this.Run   f = f()

        member this.Yield x = [x]
        member this.YieldFrom cont = cont

        member this.Combine (e, f) =
            e @ f()

        member this.While (pred, body) =
            if pred() then
                this.Combine(body(), fun () -> this.While(pred, body))
            else
                this.Zero()

        member this.For (src : #seq<_>, body) =
            use iter = src.GetEnumerator()
            this.While (iter.MoveNext, fun () -> body iter.Current)

        [<CustomOperation("query")>]
        member this.Query pred ([<ProjectionParameter>] arg) =
            // goal の引数に対する変数表を返す
            for a in pred do
                if a = arg then arg
            []

    let tiny_prolog = new TinyPrologBuilder()

    let human =
        tiny_prolog {
            yield "socrates"
        }
    let mortal =
        tiny_prolog {
            for x in human do
                yield x
        }

    (*
    let rec append =
        tiny_prolog {
            for ls in [] do
                yield ([], ls, ls)
            for (x :: xs, ls, x :: rs) in [] do
                yield! append (xs, ls, rs)
        }

    let length arg =
        tiny_prolog { function
            | Unify ([], 0) -> yield true
            | Unify (x :: xs, n) ->
                length (xs, n)
        }//*)

    let sample () =
        printfn "%A" (tiny_prolog {
             for who in mortal do
                yield who
            //query (mortal who) (fun who ->
            //    do ()
            //)
        } |> List.map (fun who -> sprintf "mortal(%s)" who))



















// gist: https://gist.github.com/vain0/a938367f5aa1512f9e21

// from: TinyProlog in Ruby
// 本稿 http://codezine.jp/article/detail/461?p=1 (Ling(2006/08/14)「Rubyで作るProlog処理系」CodeZine)
// 補記 www.oki-osk.jp/esc/prolog/in-ruby.html

// todo:
//  変数を環境ごとに一意な名前にする
//  匿名変数 (_ 相当) を作る
//  goal なのか goal list なのかの曖昧性を排除したい

// Term に対する再帰的処理がめんどくさすぎる

// 型付版も作りたい
(*
    let H = var<Term.List<'T>>("H")
    let T = var<Term.List<'T>>("T")
    let M = var<Term.Const<int>>("M")
    let length = pred<2> "length"   // Pred<2, Term.List<'T>, Term.Const<int>>
    length.[[], 0].Succeeds
    length.[H::T, N] ?- [ length.[T, M]; Goal.Call(fun env ->
        env.unify N (env.[M] + 1) )]

    すべての値に変数が許容されるのが難しそう
*)
// 複合項が自由に作れないのがもどかしい。

(*
subgoal の途中で callback 関数を呼び出せた方が便利そう
→ 単純に callback 指定の述語を挟むのでは、それの定義を探す段階でコールバックされ、渡される環境が役に立たないものになる。
(実際には subgoal の環境がほしいことが多い)
*)



open Basis.Core
open Uedai.Utilities

open System
open System.Text
open System.Collections.Generic
open System.Diagnostics
open Microsoft.FSharp.Reflection

(*
module TinyProlog =

    let mutable bTrace = false
    let iota =
        let cnt = ref (-1)
        (fun () -> incr cnt; !cnt)

    [<StructuredFormatDisplay("{ToString()}")>]
    /// 変数 (外部用)
    type Var (name : string) =
        member this.Name = name
        member this.ToString() = this.Name

    /// 環境上に実在する変数
    and VarExistence internal (refer : Var, index : int) =
        inherit Var (refer.Name)
        member this.Index = index
        member this.Equals (v : Var) = (refer = v)

        static member EqualTo (this : VarExistence, v : Var) = this.Equals v

    /// 節の定義
    and Def (head : Goal, body : IGoals) =
        let head, body, vars =
            match Term.ScanVars' [ (Goal head); (Goals body) ] with
            | [ Goal head; Goals body], vars ->
                head, body, vars
            | _ -> failwith ""

        member this.Vars = vars
        member this.Head = head
        member this.Body = body

        member this.ToString () =
            head.ToString ()
            //+   match body with
                //| [] -> "."
                //| gs -> " :- " + (gs |> List.map (fun g -> g.AsString) |> Str.join ", ") + " ."

    /// ゴールとみなせるオブジェクト
    and IGoal =
        abstract ToGoal : unit -> Goal

    /// 述語
    and Pred internal(name) =
        member val Defs = new List<Def>()

        member this.Of ()  = { Goal.Pred = this; Arg = Term.Tuple [] }
        member this.Of arg = { Goal.Pred = this; Arg = arg }
        member this.Item (prms : 'Prm) =
            this.Of(Term.Of prms)

        member this.AsString = name

        static member val Cut      = new Pred("!") with get
        static member val Succeeds = new Pred("succeeds") with get

        interface IGoal with
            member this.ToGoal () = this.Of()

    /// 述語項 (実引数を持つ述語)
    and Goal = internal {
        Pred : Pred
        Arg : Term
    } with
        member internal this.DefinePrimaryAs subgoals =
            this.Pred.Defs.Insert (0, new Def(this, subgoals))

        /// このゴールが成功するための条件を、追加で定義する
        member internal this.DefineAs subgoals =
            let def = new Def(this, subgoals)
            this.Pred.Defs.Add def
            Debug.WriteLine ("-- " + def.ToString())

        /// すべてのサブゴールが成功したときに成功する
        static member (?-) (this : Goal, subgoals : Goal list) =
            this.DefineAs (new GoalsQuery(subgoals))

        member this.Succeeds = this ?- []
        static member (?-) (this, goal : Goal) =
            this ?- [ goal ]

        // このゴールと節定義の対
        member this.WithBody goals =
            Core.if_pred (this, goals)

        member this.ScanVarsBody data =
            let arg, data' = Term.ScanVarsBody data this.Arg
            ({ this with Arg = arg }, data')

        static member (!) = Goal.Cut
        static member Cut = (Pred.Cut :> IGoal).ToGoal()

        member this.AsString =
            this.Pred.AsString
            + match this.Arg with
                | Tuple [] -> ""
                | Tuple _ | List _ | Nil -> this.Arg.ToString()
                | _ -> "(" + this.Arg.ToString() + ")"

    /// ゴール列
    and IGoals =
        abstract member Resolve : Env -> IEnumerable<bool>
        
    /// 連結ゴール
    and [<Struct>] GoalsQuery (goals : Goal list) =
        member this.Goals = goals

        interface IEnumerable<Env> with
            /// 空の環境のもとで、このゴールを解決する。
            /// 実質、Prolog の主ルーチン
            member this.GetEnumerator() =
                match Term.ScanVars' goals with
                | Goals gs, vars ->
                    use env = new Env (vars)
                    seq {
                        for q in Core.Resolve env gs do
                            Debug.Assert q
                            yield env
                    }
                | _ -> failwith ""

        interface IGoals with
            member this.Resolve (env : Env) : IEnumerable<bool> =
                Core.Resolve env goals

    /// 項
    /// タプル (a, b, ...) は扱いづらいのでリストで代用する
    and Term =
        /// 定項
        | Str of string
        | Int of int
        | Const of obj

        /// 変項
        | Var of Var
        | VarEx of VarExistence

        /// ゴール
        | Goal of Goal
        | Goals of IGoals

        /// 固定長の構造
        | Tuple of Term list

        /// Term によるリスト構造
        // [H|T] のように、尾部も項である必要があるので、標準の list では代用できない
        | List of Term * Term
        | Nil

        /// 尾部つきリスト
        // 固定長構造も、尾部に変数を含む擬似リストも表現できるが、単一化操作がやや複雑になる
        //| ListWithTail of Term list * Term option

    with
        /// makes cons cell
        static member (.|) (lhs, rhs) = List (lhs, rhs)
        /// 項がゴールとして実行されることを示す
        member this.Succeeds =
            Pred.Succeeds.[this]

        static member Of<'T> (o : 'T) : Term = Term.FromObj (o, typeof<'T>)
        static member FromObj (o : obj, typ : System.Type) =
            // list<_> 型は Term によるリストに展開する 
            if typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<list<_>> then
                let elemType = typ.GenericTypeArguments.[0]
                [ for elem in  (o :?> System.Collections.IEnumerable) do
                    yield Term.FromObj (elem, elemType) ]
                |> Term.FromList

            // _[] 型は Tuple に展開する
            elif typ.IsArray then
                let elemType = typ.GetElementType()
                Term.Tuple [ for elem in (o :?> System.Collections.IEnumerable) do
                                yield Term.FromObj (elem, elemType) ]

            // 組は Tuple に展開する
            elif typ.FullName.StartsWith("System.Tuple`") then
                let prmVals  = FSharpValue.GetTupleFields o
                let prmTypes = FSharpType.GetTupleElements(typ)
                Term.Tuple
                    [ for (prm, typ) in (Seq.zip prmVals prmTypes) do
                        yield Term.FromObj (prm, typ) ]
            else
                match o with
                | :? Goal as g -> Goal g
                | :? string as s -> Str s
                | :? int as i -> Int i
                | :? Term as t -> t
                | o -> Const o
                //| _ -> failwith "unknown term"
                
        static member FromList = function
            | [] -> Nil
            | x :: xs -> List (x, Term.FromList xs)

        /// 変数の走査
        /// 変数をすべて環境実在変数に置換した項と、それら変数の列を返す
        static member ScanVars t =
            let t, (varsRev, _) = t |> Term.ScanVarsBody ([], 0)
            let vars = varsRev |> List.rev
            // Index 値が添字になっていることを確認
            Debug.Assert (vars
                |> List.mapi (fun idx (ve : VarExistence) -> idx = ve.Index)
                |> List.forall id)
            (t, vars)

        static member internal ScanVarsBody ((vars : VarExistence list, count) as data) =
            function
            | Var v ->
                match vars |> List.tryFind (fun ve -> ve.Equals v) with
                | Some ve -> (Var ve), data
                | None ->
                    let ve = new VarExistence(v, count)
                    (VarEx ve, ((ve :: vars), count + 1))
                
            | Goal g ->
                let arg', data' = g.Arg |> Term.ScanVarsBody data
                (Goal { g with Arg = arg' }), data'
            | Goals gs as t ->
                if gs :? GoalsQuery then
                    match (Tuple (gs :?> GoalsQuery).Goals) |> Term.ScanVarsBody data with
                    | Tuple gs', data' ->
                        (Goals (GoalsQuery(gs' |> List.map (function Goal g -> g | _ -> failwith "")))), data'
                    | _ -> failwith ""
                else (t, data)

            | Tuple a ->
                let a'', data'' =
                    List.foldBack (fun (t : Term) (a, data) ->
                        let t', data' = t |> Term.ScanVarsBody data
                        (t' :: a), data
                    ) a ([], data)
                (Tuple a''), data''

            | List (h, t) ->
                let h', data'  = h |> Term.ScanVarsBody data
                let t', data'' = t |> Term.ScanVarsBody data'
                (List (h', t')), data''

            | Nil
            | Str _ | Int _
            | Const _ as t -> (t, data)

        static member internal ScanVars' (terms) =
            let t, vars = Term.ScanVars (Tuple terms)
            match t with
            | Tuple terms -> terms, vars
            | _ -> failwith ""

        static member internal ScanVars (goals : Goal list) =
            let t, vars = (Tuple (goals |> Term.Goal)) |> Term.ScanVarsBody data


        /// 変数の展開
        static member internal ExpandVar (env : Env) =
            function
            | Goal g ->
                Goal { g with Arg = g.Arg |> Term.ExpandVar env }
            | Tuple a ->
                Tuple (a |> List.map (fun t -> env.[t]))
            | List (h, t) ->
                List (env.[h], env.[t])
            
            | Var _ // ??
            | Nil
            | Str _ | Int _ | Const _ as t -> t

        /// goals にキャストする。
        /// tuple は常に goals として認めない。
        member this.CastToGoals =
            match this with
            | Goal g -> Some [g]
            | Nil -> Some []
            | List (Goal h, t) ->
                match t.CastToGoals with
                | Some t -> Some (h :: t)
                | _ -> None
            | _ -> None

        member this.ToString() =
            match this with
            | Tuple a -> "(" + (a |> List.map (fun t -> t.ToString()) |> Str.join ", ") + ")"
            | List (t, u) -> "[" + (t.ToString_List() u) + "]"
            | Nil -> "[]"
            | Goal g -> g.ToString()
            | Var v -> v.ToString()
            | Str s -> "\"" + s + "\""
            | Int n -> n.ToString()
            | Const o -> o.ToString()

        member private this.ToString_List() = function
            | List (u, v) -> this.ToString() + "; " + (u.ToString_List() v)
            | u ->
                match u with
                | Nil -> this.ToString()  // 「| []」は省略
                | _   -> this.ToString() + " | " + u.ToString()
                
        (*
        interface IEnumerable<Term> with
            member this.GetEnumerator() =
                (new TermEnumerator(this)) :> IEnumerator<Term>
            member this.GetEnumerator()  =
                (this :> IEnumerable<Term>).GetEnumerator() :> System.Collections.IEnumerator
        
    and TermEnumerator (init : Term) =
        let mutable cur = Nil
        let mutable next = init
        interface IEnumerator<Term> with
            member this.Current with get() = cur
            member this.Current with get() = cur :> obj
            member this.MoveNext() =
                match next with
                | List (t, u) -> cur <- t; next <- u; true
                | _ -> false
            member this.Reset() = cur <- Nil; next <- init
            member this.Dispose() = ()
        //*)

    /// 外部の演算とのなかだち
    // 結果だけでなく、単一化操作まで提供する
    and CallbackEnv internal(env : Env, trail) =
        member this.Item t = env.[t]

        member private this.unifyImpl unify t u =
            unify (t, env) (u, env) trail env
        member private this.resolveImpl resolve gs =
            resolve gs

    /// 環境 (変数空間、スタックフレーム)
    and Env =
        val private stack_ : List<TermEnv option>
        val private vars_  : VarExistence list
        val private top_   : int ref
        val private frame_ : int
        val internal trail_ : Trail
        val mutable internal cut_ : bool

        val mutable private disposed_ : bool

        new (vars : VarExistence list, env : Env) as this =
            {
                stack_ = env.stack_
                vars_ = vars
                top_ = env.top_
                frame_ = !env.top_
                trail_ = new Trail(env.trail_)
                cut_ = false
                disposed_ = false
            }
            then this.Initialize ()

        new (vars : VarExistence list) as this =
            {
                stack_ = new List<TermEnv option>()
                vars_ = vars
                top_ = ref 0
                frame_ = 0
                trail_ = new Trail()
                cut_ = false
                disposed_ = false
            }
            then this.Initialize ()

        member this.Initialize () =
            let sp = this.frame_ + this.vars_.Length
            this.top_ := sp
            for i in (this.stack_.Count)..(sp - 1) do
                this.stack_.Add None
            Debug.WriteLine ("+" + this.ToString())

        /// 変数に対するフレーム内のオフセット値を得る
        member private this.IndexOf (v : VarExistence) =
            let i = v.Index
            Debug.Assert (0 <= i && i < this.vars_.Length)
            i

        /// 変数表としての機能
        member internal this.Add (v, pair) =
            this.stack_.[this.frame_ + (this.IndexOf v)] <- Some pair
        
        member internal this.TryGet v =
            this.stack_.[this.frame_ + (this.IndexOf v)]

        member internal this.Remove v =
            this.stack_.[this.frame_ + (this.IndexOf v)] <- None

        interface IDisposable with
            /// スタックフレーム解放
            /// 高速化のため、底につくまでスタック自体は縮めない
            member this.Dispose() =
                if this.disposed_ then failwith ("disposed twice " + this.ToString())
                Debug.WriteLine ("- " + this.ToString())
                (this.trail_ :> IDisposable).Dispose()

                Trace.Assert (!this.top_ = (this.frame_ + this.vars_.Length),
                    this.ToString() + " is not at the top: " + (!this.top_).ToString())

                for i in 0..(this.vars_.Length - 1) do
                    this.stack_.[this.frame_ + i] <- None
                this.top_ := this.frame_
                if this.frame_ = 0 then
                    this.stack_.Clear()
                    Debug.WriteLine ("- clear " + this.ToString())

                this.disposed_ <- true

        /// 変数を可能な限り展開する
        member this.Deref t =
            let rec f (t, env : Env) =
                match t with
                | Var v ->
                    match env.TryGet v with
                    | Some p -> f p
                    | None -> (t, env)
                | _ -> (t, env)

            f (t, this)

        /// 脱参照
        // 項の中の変数をすべて、可能な限り展開する
        member this.Solve t =
            let t, env = this.Deref t
            t |> Term.ExpandVar env

        member this.Item t = this.Solve t

        /// 出力
        member this.ToString() =
            "Env " + (this.frame_).ToString() + " - " + (this.vars_.Length.ToString())
        (*
            [ for p in this.Table do
                let t, env = p.Value
                yield p.Key + " = " + env.[t].AsString ]
            |> Str.join "\r\n"
        //*)

    /// 束縛履歴
    /// バックトラックを消すための情報を保持する。
    and Trail =
        val private stack_ : List<VarExistence * Env>
        val private top_ : int ref
        val private frame_ : int
#if DEBUG
        [<DefaultValue (0)>]
        val mutable private count_ : int
#endif

        new () as this =
            {
                stack_ = new List<VarExistence * Env>()
                top_ = ref 0
                frame_ = 0
            }

        new (trail : Trail) as this =
            {
                stack_ = trail.stack_
                top_ = trail.top_
                // 既存スタックのトップにフレームを乗せる
                frame_ = !trail.top_
            }

        member internal this.Add vp =
            let sp = !this.top_
            incr this.top_
            if sp < this.stack_.Count then
                this.stack_.[sp] <- vp
            else
                Debug.Assert (this.stack_.Count = sp)
                this.stack_.Add vp
                Debug.Assert (this.stack_.Count = !this.top_)
#if DEBUG
                this.count_ <- this.count_ + 1
#endif
        interface IDisposable with
            member this.Dispose() =
                let n = !this.top_
#if DEBUG
                Debug.Assert ((n = (this.frame_ + this.count_)),
                    "Trail " + (this.frame_.ToString()) + "-" + (this.count_.ToString())
                    + " is not at the top: " + (n.ToString()) )
#endif
                for i in (this.frame_)..(n - 1) do
                    let v, env = this.stack_.[i]
                    Debug.Assert (env.TryGet v).IsSome
                    env.Remove v
                    //this.stack_.[i] <- None
                this.top_ := this.frame_
                if this.frame_ = 0 then
                    this.stack_.Clear()
                    Debug.WriteLine ("- clear " + (this.ToString()))

    and TermEnv = Term * Env

    and Core () =
        /// 変数の走査
        static member ScanVarsBody data =
            function
            | [] -> []
            | g :: g_tail ->
                let g, data' = g.ScanVars data
                
        /// 単一化
        static member Unify (xp : TermEnv) (yp : TermEnv) (trail : Trail) tmp_env =
            Core.UnifyAndOrExpand xp yp trail tmp_env

        static member private UnifyAndOrExpand xp yp trail tmp_env =
            match xp, yp with
            | (VarEx xv, x_env), (y, y_env) ->
                match x_env.TryGet xv with
                | Some (x, x_env) ->
                    Core.Unify (x_env.Deref x) yp trail tmp_env
                | None ->
                    let ((y, y_env) as yp) = y_env.Deref y
                    match y with
                    | Var yv when (xp <> yp) ->
                        x_env.Add (xv, yp)
                        if x_env <> tmp_env then
                            trail.Add (xv, x_env)
                    | _ -> ()
                    true

            | _, (Var _, _) ->
                Core.UnifyAndOrExpand yp xp trail tmp_env
            | _ ->
                Core.UnifyImpl xp yp trail tmp_env

        static member private UnifyImpl xp yp trail tmp_env =
            let unify xp yp = Core.Unify xp yp trail tmp_env

            match xp, yp with
            | (Goal xg, x_env), (Goal yg, y_env) ->
                xg.Pred = yg.Pred
                && unify (xg.Arg, x_env) (yg.Arg, y_env)

            | (Goal xgs, x_env), (Goals ygs, y_env) ->
                false

            | (Tuple xa, x_env), (Tuple ya, y_env) ->
                xa.Length = ya.Length
                && (xa, ya) ||> List.forall2 (fun x y ->
                    unify (x, x_env) (y, y_env)
                )

            | (List (xh, xt), x_env), (List (yh, yt), y_env) ->
                   (unify (xh, x_env) (yh, y_env))
                && (unify (xt, x_env) (yt, y_env))
            | (x, _), (y, _) ->
                x = y

        /// ゴールの解決
        static member Resolve env (goals : IGoals) =
            goals.Resolve env
            
        static member Resolve env (goals : Goal list) =
            match goals with
            | [] -> [true] :> IEnumerable<bool>
            | goal :: rest ->
                Debug.Indent()
                try
                    Debug.WriteLine ("?- " + goal.ToString())
                    match goals with
                    | [] ->
                        [true] :> IEnumerable<bool>
                    | goal :: rest ->
                        if goal.Pred.Defs.Count = 0 then
                            failwith (goal.Pred.ToString() + " is undefind.")
                        else
                            seq {
                                for df in goal.Pred.Defs do
                                    use d_env = new Env (df.Vars, env)
                                    if ( Core.Unify (Goal goal, env) (Goal df.Head, d_env) d_env.trail_ d_env ) then
                                        Debug.WriteLine ("- " + goal.ToString() + " ~ " + df.Head.ToString())

                                        yield! seqBreakable {
                                            for q in (df.Body :> IGoals).Resolve d_env do
                                                if not q then
                                                    Debug.WriteLine ("-cut " + env.ToString())
                                                    env.cut_ <- true
                                                    break_
                                                for r in (rest :> IGoals).Resolve env do
                                                    Debug.Assert r
                                                    if env.cut_ then break_
                                                    return true
                                                if env.cut_ then break_
                                        }
                                    else
                                        Debug.WriteLine ("- " + goal.ToString() + " !~ " + df.Head.ToString())
                            }
                finally
                    Debug.Unindent()
                
        /// 読みやすい文字列表現
        static member ToString x =
            ""

        /// 組み込み述語
        static member val Fail =
            { new IGoals with
                member this.Resolve env = ([false] :> IEnumerable<bool>)
            }

    let inline andThen task e =
        let result = e
        task ()
        result

    let inline andThen' task e =
        task e ; e

    type CallbackEnv with
        member this.unify t u = this.unifyImpl unify t u
        member this.resolve gs = this.resolveImpl resolve gs

    // public
    let pred name = new Pred(name)
    let var name  = new Var(name)

    type TinyPrologBuilder () =
        member this.Return x = x
        member this.Yield x = x

    let tiny_prolog = new TinyPrologBuilder()

open TinyProlog

let sample () =
    let A, B, C, X, Y, Z = var "A", var "B", var "C", var "X", var "Y", var "Z"
    let N, M = var "N", var "M"

    let demandAll (env : Env) =
        printfn "%s ;" env.AsString
        false   // means 'no', or ';'

    let (~?-) (g : Goal) =
        printfn "?- %s" g.AsString
        resolve [g]

    let append = pred "append"
    append.[[], Z, Z].Succeeds
    append.[A.|X, Y, A.|Z] ?- append.[X, Y, Z]

    (?- append.[[1], [2], X]) demandAll |> ignore

    // 非決定性
    // (X, Y) = ([], [1; 2]), ([1], [2]), ([1; 2], [])
    (?- append.[X, Y, [1; 2]]) demandAll |> ignore

    // callback 関数を使用する例
    let subt = pred "subt"
    subt.[X, Y, Z] ??- (fun env ->
        match env.[X], env.[Y], env.[Z] with
        // 左から右への処理
        | Int x, Int y, _ ->
            env.unify Z (Term.Int (x - y))

        // 双方向性のための処理 (2変数以上はお手上げ)
        | _, Int y, Int z -> env.unify X (Term.Int (y + z))
        | Int x, _, Int z -> env.unify Y (Term.Int (x - z))
        | _ -> false
    )
    (?- subt.[Int 3, Int 1, A]) demandAll |> ignore
    // A = 2
    (?- subt.[A, Int 1, Int 2]) demandAll |> ignore
    // A = 3

    let length = pred "length"
    (*
    length.[[], 0].Succeeds
    length.[List(X, Y), N] ?- [length.[Y, M]; Goal.Call (fun (env_subgoals, cb_env) ->
        match env_subgoals.[M] with
        | Int m -> env_subgoals.unify N (Int (m + 1))
        | _ -> false
    )]
    length.[List(X, Y), N] ??- (fun env ->
        env.resolve [length.[Y, M]] (fun envInner ->
            match envInner.[M] with
            | Int m -> env.unify N (Int (m + 1))
            | _ -> false
        )
    )
    //*)
    //length.[List(X, Y), N] ?- [ length.[Y, M], N .<- (fun m -> m + 1) ]
    //(?- length.[Term.OfList [1; 2], N]) demandAll |> ignore

    // ゴール項を実行する例
    (*
    // ゴール項を実行する述語
    Pred.Succeeds.[A] ??- (fun env ->
        let a = env.[A]
        match env.[A].CastToGoals with
        | Goal g -> env.resolve [g] (fun envInner -> )
        | _ -> false
    )
    //*)

    let neg = pred @"\+"
    neg.[A] ?- [A.Succeeds; Goal.(!); Goal.Fail]
    neg.[A].Succeeds
    let (/+) a = neg.[a]

    (?- neg.[append.[[], X.|Y, []]]) demandAll |> ignore
    // → 成功

    let unify  = pred "unify"
    unify.[X, X].Succeeds
    let (.=) x y = unify.[x, y]
    let (.<>) x y = neg.[ x .= y ]

    //(?- (X .=  1)) demandAll |> ignore
    //(?- (1 .=  2)) demandAll |> ignore
    //(?- (X .<>  1)) demandAll |> ignore
    //(?- (1 .<>  2)) demandAll |> ignore
    
    // カットを使う例
    let remove = pred "remove"
    remove.[[], B, []].Succeeds
    remove.[List(B, X), B, Y] ?- [Goal.(!); remove.[X, B, Y]]
    remove.[List(A, X), B, List(A, Y)] ?- remove.[X, B, Y]

    (?- remove.[[1; 2], 2, X]) demandAll |> ignore
    //=> X = [1]
    // カットしなければ X = [1; 2] も出てくる

    // メタ述語
    // findall.[変数X, ゴールリストGs, リストXs]
    let findall = pred "findall"
    findall.[X, A, Y] ??- (fun env ->
        let result = new List<Term>()
        match env.[A].CastToGoals with
        | Some gs ->
            env.resolve gs (fun (envInner : Env) ->
                result.Add envInner.[X]; false) |> ignore
            env.unify Y (Term.FromList (result |> List.ofSeq))
        | _ ->
            failwith "findall's second argument must be goal."
            false
    )
    (?- findall.[X, append.[X, Y, [1]], Y]) demandAll |> ignore

    let contains = pred "contains"
    contains.[A.|B, A].Succeeds
    contains.[B.|X, A] ?- contains.[X, A]

    //(?- contains.[[1; 2; 3], 3]) demandAll |> ignore
    //(?- contains.[[1; 2; 3], 4]) demandAll |> ignore
    (?- contains.[[1; 2; 3], X]) demandAll |> ignore
    (?- contains.[[X; Y; Z], 7]) demandAll |> ignore

    let remove' = pred "remove'"
    remove'.[X, A, Y] ?- findall.[B, [ contains.[X, B]; A .<> B ], Y]

    (?- remove'.[ [1; 2; 3; 1; 2], 2, X ]) demandAll |> ignore
    (?- remove'.[ [[1; 2]; [1; 3]; [2; 3]], [Term.Int 1; A], X ]) demandAll |> ignore
    //→ X = [[2; 3]]

    (*
    let s = pred "sample"
    s.[__, __] ?- []
    (?- s.[1, 1]) demandAll |> ignore
    (?- s.[1, 2]) demandAll |> ignore
    *)

    ()
    (*
    (append^[ Nil; Z; Z ]).si []
    (append^[ List(A, X); Y; List(A, Z)]).si [ append^[X; Y; Z] ]
    resolve [ append^[ List(Int 1, Nil); List(Int 2, Nil); X] ] (fun env ->
        printfn "%s" env.AsString
        false
    )
    |> ignore
    *)
    //*)

//*)
