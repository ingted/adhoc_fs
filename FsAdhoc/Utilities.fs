namespace Uedai.Utilities

// おせっかいな関数群

open System
open System.IO
open System.Collections.Generic
open System.Linq

open Basis.Core

open Microsoft.FSharp
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Reflection

open FParsec

[<AutoOpen>]
module Misc =
  /// constant function
  let inline konst x _ = x

  ///flip applying order of 2 arguments
  let inline flip f x y = f y x

  let inline curry2 f x y = f (x, y)

  ///apply f to a value x; then return x
  let tap f x =
      do f x
      x

  /// メモ化
  let memorize (f : 'a -> 'b) =
      let memo = Map.empty
      let memorizedFunc (a : 'a) : 'b =
          match memo.TryFind a with
          | Some b -> b
          | None -> f a |> tap (fun b -> memo.Add (a, b) |> ignore)
      memorizedFunc

  /// 非負最小剰余; pをqで割った余りを非負値で返す
  let inline positiveModulo p q =
      (p % q + q) % q

  /// 型つき ignore
  let inline ignore'<'T> (_ : 'T) = ()

  /// 返却値宣言の ignore
  let inline asserts pred given =
      assert (pred given); ()

  ///パイプライン中でのassert
  let inline assert' pred given =
      assert (pred given); given

  let if' zero proc cond =
      if cond then proc() else zero

  let alwaysTrue _ = true

  ///expands KeyValuePair<_,_> to a tuple (key, value)
  let (|KeyValuePair|) (kv : KeyValuePair<'K, 'V>) =
      (kv.Key, kv.Value)

module Math =
  let numDigits n =
      if n = 0
      then 1
      else n |> Math.Abs |> float |> Math.Log10 |> int |> (+) 1

module Tuple =
  let fst'3 (t0, t1, t2) = t0
  let snd'3 (t0, t1, t2) = t1
  let thr'3 (t0, t1, t2) = t2

module Option =
  let union' (proc: unit -> 'T option) = function
      | Some x -> Some x
      | None -> proc()

  let intersect' proc = function
      | Some x -> proc()
      | None -> None

  /// mplus in MonadPlus, or (<|>) in Alternative
  ///operator || in javascript; ordered, not shortcircuit
  let union opt2 = union' (fun() -> opt2)

  ///operator && in javascript; ordered, not shortcircuit
  let intersect opt2 = intersect' (fun() -> opt2)

  ///from bool to option
  let if' (b: bool) (proc: unit -> 'T) =
      if b then Some (proc ()) else None

  ///wrap in option with predcate
  let ifSat (pred: 'T -> bool) (x: 'T) =
      if pred x then Some x else None

  ///wrap a nullable non-null value in Some if non-null, otherwise None
  let ofNullable (x : 'T when 'T : null) = 
      x |> ifSat ((<>) null)

  ///(success, value) -> Some value or None
  let trialResult (b: bool, value) =
      if b then Some value else None

  ///seq.filter
  let filter pred = Option.bind (ifSat pred)
  let toSeq opt = opt |> Option.toList |> Seq.ofList

module Seq =
  let iota n = seq { 0..(n-1) }

  //optional getters
  let tryFirst self =
      Option.if' (self |> Seq.isEmpty |> not) (fun() -> self |> Seq.head)

  let tryLast self =
      Option.if' (self |> Seq.isEmpty |> not) (fun() -> self |> Seq.last)

  let tryNth n self =
      if n < 0
      then None
      else
        ((n, None), self)
        ||> Seq.fold (fun (n, result) x ->
              if n = 0 then (-1, Some x) else (n - 1, result)
            ) |> snd

  let tryMax self = Option.if' (self |> Seq.isEmpty |> not) (fun() -> self |> Seq.max)

  /// 添字付け (自然数列との zip)
  let indexed self = self |> Seq.zip (self |> Seq.length |> iota)

  /// 含んでいるか否か
  let contains value self =
      self |> Seq.exists ((=) value)

  /// n 要素回転させる 
  let rotate n self =
      if self |> Seq.isEmpty
      then self
      else
        let n = positiveModulo n (self |> Seq.length)
        Seq.append (self |> Seq.skip n) (self |> Seq.take n)

  /// スライス
  (*
  let slice' idxBgnOpt idxEndOpt self =
      let idxBgn = idxBgnOpt |> Option.getOr 0
            
      let self = self |> Seq.skip idxBgn
      match idxEndOpt with
      | Some idxEnd -> self |> Seq.take (idxEnd - idxBgn)
      | None -> self//*)

  /// product of two sequences.
  /// for example, product2 [1; 2; 3] [a; b] becomes seq { (1, a); (1, b); (2, a); (2, b); (3, a); (3, b) }
  let product2 l0 l1 =
      seq { for e0 in l0 do for e1 in l1 do yield (e0, e1) }

  let product3 l0 l1 l2             = seq { for e0 in l0 do for e1 in l1 do for e2 in l2 do yield (e0, e1, e2) }
  let product4 l0 l1 l2 l3          = seq { for e0 in l0 do for e1 in l1 do for e2 in l2 do for e3 in l3 do yield (e0, e1, e2, e3) }
  let product5 l0 l1 l2 l3 l4       = seq { for e0 in l0 do for e1 in l1 do for e2 in l2 do for e3 in l3 do for e4 in l4 do yield (e0, e1, e2, e3, e4) }
  let product6 l0 l1 l2 l3 l4 l5    = seq { for e0 in l0 do for e1 in l1 do for e2 in l2 do for e3 in l3 do for e4 in l4 do for e5 in l5 do yield (e0, e1, e2, e3, e4, e5) }
  let product7 l0 l1 l2 l3 l4 l5 l6 = seq { for e0 in l0 do for e1 in l1 do for e2 in l2 do for e3 in l3 do for e4 in l4 do for e5 in l5 do for e6 in l6 do yield (e0, e1, e2, e3, e4, e5, e6) }
        
  module SeqExtension =
      //type IEnumerable<'T> with
      //    member this.GetSlice (iBgn, iEnd) = slice' iBgn iEnd this
      ///ref http://stackoverflow.com/questions/14121556/overloading-measure-operator?rq=1
      type Mult = Mult with
          static member        ($) (Mult, x: _ option) = fun f -> Option.map f x
          static member        ($) (Mult, x: _ list)   = fun f -> List.map f x
          static member        ($) (Mult, x: _ array)  = fun f -> Array.map f x
          //static member inline ($) (Mult, x: #seq<_>)  = fun f -> Seq.map f x
          static member ($) (Mult, x: Mult) = fun () -> Mult //dummy overload
      let inline (|>>) x f = (Mult $ x) f
      //let ``test fmap operator`` =
          //([1;2;3] |>> string)
      ()
  (*
  let cast (self : #IEnumerable<_>) =
      let ie = self.GetEnumerator()
      seq { while ie.MoveNext() do yield ie.Current }
  *)

  open Basis.Core.ComputationExpr
  type SeqWithFlowControlBuilder internal () =
      member this.Zero() = Seq.empty, Continue

      member this.Yield(x) = Seq.singleton x, Continue
      member this.YieldFrom(xs) = xs, Continue
      member this.Return(x) = Seq.singleton x, Break
      member this.ReturnFrom(xs) = xs, Break

      member this.Delay f = f
      member this.Run f = f() |> fst

      member this.Bind(x, f) =
          (Seq.collect (f >> fst) x, Continue)
      member this.Using(x : #IDisposable, f) =
          try f x
          finally
            if x <> null then x.Dispose()

      member this.Combine((xs, flow), cont) =
          match flow with
          | Break -> xs, Break
          | Continue ->
              if xs |> Seq.isEmpty
              then cont()
              else (xs, Break) // ??

      member this.TryWith(f, handler) =
          try f()
          with e -> handler e
      member this.TryFinally(f, compensation) =
          try f()
          finally compensation()
      member this.While(pred, f) =
          if pred() then
              this.Combine(f(), fun () -> this.While(pred, f))
          else this.Zero()
      member this.For(src : #seq<_>, f) =
          this.Using
            ( src.GetEnumerator()
            , (fun iter -> this.While(iter.MoveNext, (fun () -> f iter.Current)))
            )

      [<CustomOperation("break_")>]
      member this.Break (x, _) = (x, Break)
      //[<CustomOperation("continue")>]
      //member this.Continue (xs, _) = (x, Continue)

  let seqBreakable = new SeqWithFlowControlBuilder()

module List =
  let maxWithIndex f =
      Seq.indexed >> List.ofSeq >> (List.maxBy f)

  /// product of lists, but these lists must be same types, and result list contains concrete length lists.
  //(*
  let product self =
      List.foldBack (fun head tailProd ->
          [ for h in head do  for tail in tailProd do  yield h :: tail ]
      ) self [[]]
  //*)
  let takeTuple3 self = (List.nth self 0), (List.nth self 1), (List.nth self 2)

  let tryAssoc key self =
      let f l (k, v) =
          let r = Option.if' (k = key) (fun () -> v)
          (l, r) ||> Option.union
      self |> List.fold f None

  // tryAssoc (unzipped version)
  let tryAssocUnzip key (keys, vals) =
      keys
      |> List.tryFindIndex ((=) key)
      |> Option.map (fun i -> vals |> Seq.nth i)

module Array =
  let inline ofCol< ^T, ^U when ^T: (member Count: int) and ^T: (member Item: int -> ^U)> (o: ^T): ^U[] =
      [| for i in 0..((^T: (member Count: int) o) - 1) do
          yield (^T: (member Item: int -> ^U) (o, i)) |]

  let take n a =
      Array.sub a 0 n
  let drop n a =
      Array.sub a 1 (Array.length a - 1)

module Str =
  let (|Empty|Cons|) (str : string) =
      if str.Length = 0
      then Empty
      else Cons(str.[0], str.[1..])

  let private optFromIndex i =
      Option.if' (i >= 0) (fun () -> i)

  let tryIndexOf     target = (Str.indexOf target) >> optFromIndex
  let tryLastIndexOf target = (Str.lastIndexOf target) >> optFromIndex

module Integer =
  let (|Positive|Zero|Negative|) i =
    if   i = 0 then Zero
    elif i > 0 then Positive i
    else Negative i

module Ref =
  let apply' f r = (r := (f !r))
  //let inline (+=) r n = r |> apply' ((+) n)
  //let inline (-=) r n = r |> apply' ((-) n)
  let inc = apply' ((+) 1)
  let dec = apply' ((-) 1)
        
[<AutoOpen>]
module RegexExtension =
  open System.Text.RegularExpressions

  (* 
  let (|Match|_|) pattern input =
      let m = Regex.Match(input, pattern)
      Option.if' m.Success (fun() -> m)
  /// マッチしたグループの値の列を返す
  let (|MatchBind|_|) pattern input =
      let m = Regex.Match(input, pattern)
      Option.if' m.Success (fun() ->
          [ for group in m.Groups.Cast<Group>() do
              if group.Success then
                  yield group.Value ])
  //*)
  let (|Matches|) pattern input =
      let m = Regex.Matches(input, pattern)
      m.Cast<Match>()

  type Group with
      member this.TryValue = Option.if' this.Success (fun() -> this.Value)

/// 輪環の順でソート
module SortAsCyclicOrder =
  let sortAsCyclicOrder (cycle : 'T list) =
      let lenCycle  = cycle |> List.length
      let indexOf x = cycle |> List.findIndex ((=) x)

      let cycleData (src : 'T list) =
          let src' = src |> List.map indexOf

          // cycle.[idx] が与列に含まれているか否か
          let isGiven idx = src' |> Seq.exists ((=) idx)

          let cycleBools = [ for i in 0..(lenCycle - 1) do yield isGiven i ]

          // 与えられた要素の種類の数 (true の数を数える)
          let cntKinds =
              cycleBools |> Seq.filter id |> Seq.length
              //|> Seq.countBy id |> Seq.tryFind (fst >> ((=) true)) |> Option.getOr (true, 0) |> snd

          cycleBools, cntKinds, src'
        
      // cycle の、idx から始まる弧の長さ
      let arcLengthFrom idx cntKinds (cycleBools : bool list) =
          let len, cntOccursion =
              if not cycleBools.[idx] // 弧の最初が true でなければ最小ではありえないので無視
              then (0, 0)
              else
                cycleBools
                |> Seq.rotate idx
                |> Seq.fold
                    (fun (len, cnt) b ->
                      if cnt = cntKinds
                      then (len, cnt)
                      else (len + 1, (if b then cnt + 1 else cnt))
                    ) (0, 0)
          Option.if' (cntOccursion = cntKinds) (fun () -> len)

      // 最短弧の中で最左なものの位置
      let leftestArc cntKinds cycleBools =
          [ for i in 0..(lenCycle - 1) do
              match (arcLengthFrom i cntKinds cycleBools) with
              | Some len -> yield len, i
              | None -> ()
          ]
          |> Seq.min // 辞書式順序 (より短いもの、次いで、より idx の小さい(左の)もの)

      let sort (src : 'T list) =
          let cycleBool, cntKinds, src' = src |> cycleData
          let _, idxArc = leftestArc cntKinds cycleBool
          src |> Seq.rotate idxArc
      sort

  // カラーパイ整列
  (*
  let sortAsColorpie =
      sortAsCyclicOrder allColorAtoms
  //*)

module BoolJunctionBuilder =
  type BoolJunctionBuilder private (bNeg : bool) =
      let (!!) = (<>) bNeg

      member __.Zero()   = false
      member __.Yield  x = !! x
      member __.Return x = !! x
      member __.Run    r = !! r()
      member __.Delay  f = f

      member __.Bind (x, k) = k x // 無意味？
      member this.Using (r : #IDisposable, f) =
          try f r
          finally (match box r with | null -> () | _ -> r.Dispose())

      member __.Combine (x, k) =
          x && k()
      member __.TryWith (f, handler) =
          try f() with | e -> handler e
      member __.TryFinally (f, compensation) =
          try f() finally compensation()

      member this.While (pred, body) =
          pred() && body() && this.While (pred, body)
      member this.For (src : #seq<_>, body) =
          this.Using
            ( src.GetEnumerator()
            , (fun iter -> this.While (iter.MoveNext, (fun() -> body iter.Current)))
            )

      static member Conjunction = BoolJunctionBuilder false
      static member Disjunction = BoolJunctionBuilder true
        
  let conj = BoolJunctionBuilder.Conjunction
  let disj = BoolJunctionBuilder.Disjunction

module Map =
  /// list of keys in the map
  let keysIf pred map =
      map
      |> Map.filter pred
      |> Map.toSeq
      |> Seq.map fst

  let keys map =
      map |> keysIf (fun _ -> alwaysTrue)

  /// listof all values in the map, including duplications
  let valuesIf pred map =
      map
      |> Map.filter pred
      |> Map.toSeq
      |> Seq.map snd

  let values map =
      map |> valuesIf (fun _ -> alwaysTrue)

  let image map =
      values map

  ///find all keys whose value equals to the given value
  let inverse value map =
      map |> keysIf (fun _ -> (=) value)

  let toDictionary (self : Map<'Key, 'Value>) =
      let dict = new Dictionary<'Key, 'Value>()
      for (KeyValuePair kv) in self do
          dict.Add kv
      dict

/// represents multiset. Immutable, structural euqlity, only partial comparison.
type MultiSet<'T when 'T : comparison> internal(m : Map<'T, int>) (*as this*) =
  //let repeatedArray = lazy this.RepeatedArray() // for hashcode

  member this.ToMap() = m
  member this.ToSet() = Set.ofSeq (m |> Map.keys)

  // constructors
  new() =
      new MultiSet<'T>(Map.empty)
  new(ms : MultiSet<'T>) =
      new MultiSet<'T>( (ms.ToMap() : Map<'T, int>) )

  override this.ToString() =
      m.ToString()

  override this.Equals(obj) =
      match obj with
      | :? MultiSet<'T> as another -> this.Equals'(another)
      | _ -> false

  override this.GetHashCode() =
      m.GetHashCode()

  /// make sequence from multiset; such as {| a: 2, b: 1 |} -> seq { a; a; b }
  member internal this.RepeatedSeq() =
      seq {
        for KeyValuePair (e, n) in m.ToArray() do
          for i in 0..(n - 1) -> e
      }

  interface IEnumerable<KeyValuePair<'T, int>> with
    member this.GetEnumerator() =
        (this.ToMap() :> IEnumerable<KeyValuePair<'T, int>>).GetEnumerator()
    member this.GetEnumerator() =
        (this :> IEnumerable<KeyValuePair<'T, int>>).GetEnumerator()
        :> Collections.IEnumerator

  ///count of elem in mset
  member this.Count (elem) =
      m.TryFind elem |> Option.getOr 0

  member this.IsSubset (another: MultiSet<'T>) =
      m |> Map.forall (fun elem n ->
          n <= (another.Count elem)
      )

  member internal this.Equals' (another: MultiSet<'T>) =
      this.IsSubset(another) && another.IsSubset(this)

module MultiSet =
  let count elem (ms : MultiSet<_>) =
      ms.Count elem
  let contains elem ms =
      (ms |> count elem) > 0

  /// the empty multiset
  let empty<'T when 'T : comparison> = new MultiSet<'T>()

  let ofSet(set: Set<'T>) =
      new MultiSet<'T>([ for e in set -> (e, 1) ] |> Map.ofList)

  let internal ofDictionary(dict : Dictionary<'T, int>) =
      new MultiSet<'T>([ for KeyValuePair kv in dict -> kv ] |> Map.ofList)

  let ofSeq(sq : seq<'T>) =
      let dict = new Dictionary<'T, int>()
      for elem in sq do
          match dict.TryGetValue elem with
          | true,  n -> dict.[elem] <- (n + 1)
          | false, _ -> dict.Add(elem, 1)
      dict |> ofDictionary

  let toMap (ms: MultiSet<'T>) = ms.ToMap()
  let toSeq (ms: MultiSet<'T>) = ms.RepeatedSeq()
  let toList  ms = ms |> toSeq |> List.ofSeq
  let toArray ms = ms |> toSeq |> Array.ofSeq

  //2項演算
  ///op key n m: キーkeyの重複度がn,mであるときの、演算結果のkeyの重複度
  let internal binaryPairwiseOperate op another self =
      let dict = new Dictionary<'T, int>()
      let yield' key n m =
          let k = op key n m
          assert (k >= 0)
          if k > 0 then dict.Add (key, k)

      for KeyValuePair (key, n) in self do
          yield' key n (another |> count key)
      for KeyValuePair (key, m) in another do
          if (self |> count key) = 0 then
              assert ((self |> count key) = 0)
              yield' key 0 m
      dict |> ofDictionary

  ///和集合
  let union      another = binaryPairwiseOperate (fun _ -> (+)) another
  ///共通部分
  let intersect  another = binaryPairwiseOperate (fun _ -> min) another
  ///差集合
  let difference another = binaryPairwiseOperate (fun _ lhs rhs -> (lhs - rhs) |> max 0) another

  let equals another (self: MultiSet<'T>) = self.Equals'(another)
  ///部分集合
  let isSubset another (self: MultiSet<'T>) = self.IsSubset(another)
  let isSuperset another self = another |> isSubset self

module Serialize =
  open System.Runtime.Serialization
  open System.Runtime.Serialization.Json

  // https://gist.github.com/theburningmonk/2071722
  let private toString = System.Text.Encoding.UTF8.GetString
  let private toBytes (x : string) = System.Text.Encoding.UTF8.GetBytes x

  let serializeJson<'a> (x : 'a) = 
      let jsonSerializer = new DataContractJsonSerializer(typedefof<'a>)

      use stream = new IO.MemoryStream()
      jsonSerializer.WriteObject(stream, x)
      toString <| stream.ToArray()

  let deserializeJson<'a> (json : string) =
      let jsonSerializer = new DataContractJsonSerializer(typedefof<'a>)

      use stream = new IO.MemoryStream(toBytes json)
      jsonSerializer.ReadObject(stream) :?> 'a

module Encoding =
  let Shift_JIS = Text.Encoding.GetEncoding("Shift_JIS")

module Path =
  // ファイル名に使用できない文字をエスケープする
  let Escape escaper path =
      let rec esc path c =
          path |> Str.replace (string c) (escaper c)

      Path.GetInvalidFileNameChars()
      |> Array.fold esc path

module Console =
  let ReadCommandLine argv =
      if argv |> Array.length > 0
      then argv
      else
        printfn "Input command line. You can just use space-separated arguments:"
        let commandLine = Console.ReadLine()
        commandLine.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)

  // y/n 質問
  let rec ReadYesNo() =
      let s = Console.ReadLine()
      if s |> Str.isNullOrEmpty
      then ReadYesNo()
      else s.[0] |> Char.ToLower |> ((=) 'y')

  /// 列のどの1つかをユーザに選択させる
  let inline AskWhichOne msg s =
      match s |> Seq.length with
      | 0 -> None
      | 1 -> Some (0, s |> Seq.head)
      | len ->
          s |> Seq.iteri (fun i e ->
              printfn "%0*d  %s" (Math.numDigits len) i (string e)
              )
          printfn "%s" msg

          let rec askLoop kont =
              printf "Input a number 0~%d or '!' to skip: " (len - 1)
              let answer = Console.ReadLine()
              if answer = "!"
              then kont None
              else
                match Int32.TryParse answer |> Option.trialResult with
                | Some n when 0 <= n && n < len -> kont <| Some n
                | _ ->
                    askLoop kont

          askLoop <| Option.map (fun i -> (i, s |> Seq.nth i))

  /// 入力を行区切りですべて取り出す
  let rec ReadLines() =
      Seq.unfold
        (fun () ->
          let s = Console.ReadLine()
          Option.if' (s <> null) (fun() -> (s, ()))
        ) ()

[<AutoOpen>]
module Reflection =
  type DU<'T> () =
    static member val CaseInfos =
        FSharpType.GetUnionCases(typeof<'T>)
        |> Array.toList

    static member val Names =
        DU<'T>.CaseInfos
        |> List.map (fun (case: UnionCaseInfo) -> case.Name)

    static member val UnitCases =
        DU<'T>.CaseInfos
        |> List.choose (fun ci ->
            if ci.GetFields().Length = 0
            then Some (FSharpValue.MakeUnion(ci, Array.empty) :?> 'T)
            else None
          )

    static member FromString str =
        let caseOpt =
            DU<'T>.CaseInfos
            |> List.tryFind (fun case -> case.Name = str)
        match caseOpt with
        | Some case -> FSharpValue.MakeUnion (case, [||])
        | None -> failwith ("unknown case of " + typeof<'T>.Name)

[<AutoOpen>]
module FParsec =
  type Parser<'T> = Parser<'T, unit>

  module ErrorMessageList =
    let rec toList (self: ErrorMessageList) =
        if self = null
        then []
        else (self.Head :: toList self.Tail)

  /// 改行ではない空白類
  let blankMany: Parser<_> =
      manyChars (anyOf " \t　")

  let blankMany1 =
      notEmpty blankMany

  let skipEmptyLine =
      blankMany .>> skipNewline
      |>> ignore

  /// 改行を除く任意の文字列
  let anyInLine: Parser<_> =
      manyChars (noneOf "\r\n")
