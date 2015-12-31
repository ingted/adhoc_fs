﻿namespace MTG

open System
open System.Text
open Basis.Core
open Uedai.Utilities

// 2014/03/17 現在のルール

[<AutoOpen>]
module Core =
  // 基本構造

  type Natural = uint32
  type NumOrVar =
    | Num of Natural
    | Var of string

  // 色
  type ColorAtom =
    | White | Blue | Black | Red | Green
  with
    static member Cases =
        Reflection.DU<ColorAtom>.UnitCases
        
    static member op_Explicit (this) =
        match this with
        | White -> 'W'
        | Blue  -> 'U'
        | Black -> 'B'
        | Red   -> 'R'
        | Green -> 'G'

  type Color = Set<ColorAtom>
  let colorless = new Color([])
  let monocolor ca = new Color([ca])
  let colorFromAtoms cas = new Color(cas)

  let (|Colorless|MonoColored|MultiColored|) (clr : Color) =
      match clr.Count with
      | 0 -> Colorless
      | 1 -> MonoColored (List.nth (clr |> Set.toList) 0)
      | _ -> MultiColored clr

  /// 「支払えるか否か」を基準として考える色。
  /// つまり、混成と多色を区別する。命題論理式に似ている。
  // TODO: 順番や重複を修正するプロパティ
  type ColorPayable =
    | Colorless
    | Atom of ColorAtom
    | Or  of ColorPayable list
    | And of ColorPayable list
  with
    static member toColor (this : ColorPayable) = this.ToColor()
    member this.ToColor() =
        match this with
        | Colorless -> colorless
        | Atom ca -> monocolor ca
        | Or cps | And cps
            -> Set.unionMany (cps |> List.map ColorPayable.toColor)

    // 結合子
    member private this.Combinator =
        match this with
        | Or _  -> Some ColorPayable.Or
        | And _ -> Some ColorPayable.And
        | _ -> None

    // 標準形 (吸収律などを適用して冗長性を除去する、順番は適正化しない)
    member this.NormalForm =
        match this with
        | Colorless | Atom _ -> this
        | Or  cps -> (cps |> ColorPayable.NormalFormOfCps true  Or)
        | And cps -> (cps |> ColorPayable.NormalFormOfCps false And)

    static member private NormalFormOfCps isOr combinator cps =
        match cps |> Seq.distinct |> List.ofSeq with
        | [] -> Colorless
        | [ cp ] -> cp.NormalForm
        | cps ->
            // Or [Or [...]; ...] のように入れ子になっているのは展開する
            combinator (cps |> List.collect (fun cp ->
                match cp.NormalForm with
                | Or  cpsInner when     isOr -> cpsInner
                | And cpsInner when not isOr -> cpsInner
                | cp -> [ cp ]
            ))
                
    // 選言標準形
    member this.DisjNormalForm =
        this.NormalForm.DisjNormalForm'
    member private this.DisjNormalForm' =
        match this with
        | Colorless | Atom _ -> this //.NormalForm
        | Or cps -> Or (cps |> List.map (fun cp -> cp.DisjNormalForm))
        | And cps -> ColorPayable.DNF_AndCps cps

    // 分配律を使って And を内側に送り込む
    static member private DNF_AndCps cps =
        cps
        |> List.map (fun cp -> cp.DisjNormalForm)
        |> List.map (function
            | Or cpsOr -> cpsOr
            | cp -> [cp] )
        |> List.product
        |> List.map And
        |> Or

  // タイプ
  type Supertype =
    | Basic
    | Legendary
    | Snow
    | World
  with
    static member Cases =
        Reflection.DU<Supertype>.UnitCases

  type CardType =
    | Artifact
    | Creature
    | Enchantment
    | Land
    | Planeswalker
    | Instant
    | Sorcery
    | Tribal
  with
    static member Cases =
        Reflection.DU<CardType>.UnitCases
    static member op_Explicit (this) =
        match this with
        | Artifact     -> 'A'
        | Creature     -> 'C'
        | Enchantment  -> 'E'
        | Land         -> 'L'
        | Planeswalker -> 'P'
        | Instant      -> 'I'
        | Sorcery      -> 'S'
        | Tribal       -> 'T'

  // シンボル
  type ManaSymbol =
    /// {0}, {1}, ..., {15}, ...
    | NumManaSymbol of Natural
    /// {X}, {Y}, {Z}, or another
    | VarManaSymbol of string
    /// {W}, {U}, {B}, {R}, {G}
    | ColorManaSymbol of ColorAtom
    /// {A/B}, such as {W/U}, {2/B}, {R/P}
    | HybridManaSymbol of ManaSymbol * ManaSymbol
    /// {S}
    | SnowManaSymbol
    /// represents "2 life"; This's used as half of HybridManaSymbol, to represent Phyrexian Mana Symbol.
    | TwoLifeSymbol
    /// {Symbol/2}, such as {白/2} (ex. 《Little Girl》)
    | HalfManaSymbol of ManaSymbol
    /// {∞} (ex. 《Mos Lotus》)
    //| InfiniteManaSymbol
  with
    static member (/) (lhs, rhs) =
        HybridManaSymbol (lhs, rhs)
                
    static member PhyrexianManaSymbol clra =
        (ColorManaSymbol clra) / TwoLifeSymbol

    member this.ColorPayable =
        match this with
        | ColorManaSymbol ca ->
            ColorPayable.Atom ca
        | HybridManaSymbol (lhs, rhs) ->
            ColorPayable.Or [lhs.ColorPayable; rhs.ColorPayable]
        | HalfManaSymbol src ->
            src.ColorPayable
        | NumManaSymbol _
        | VarManaSymbol _
        | SnowManaSymbol
        | TwoLifeSymbol ->
            ColorPayable.Colorless
    member this.Color =
        this.ColorPayable.ToColor()

  type ManaCost = ManaSymbol list //Bag<ManaSymbol>
  let colorFromManacost (mc : ManaCost) =
      mc |> List.fold (fun clr sym -> clr + sym.Color) colorless

  type PhaseSymbol =
    | TapSymbol
    | UntapSymbol
  with
    static member Cases =
        Reflection.DU<PhaseSymbol>.UnitCases

  type LoyaltySymbol =
    | LoyaltySymbol of (NumOrVar * (int -> int))

// MTG の静的表現、つまりゲーム外の状態を表現する型
[<AutoOpen>]
module Static =
  open Core

  type Expansion = {
      FullName : string
      ShortName : string  // 3 characters
    }

  type Rarity =
    | Common | Uncommon | Rare | MythicRare
  with
    static member Cases =
        Reflection.DU<Rarity>.UnitCases

  type SplitCard = Spec list

  /// represents a "physical" card
  and Card =
    | RegularCard     of Spec
    | FlipCard        of Spec * Spec

    /// FrontFace, BackFace
    | DoubleFacedCard of Spec * Spec
        
    /// LeftHalf, RightHalf
    | SplitCard       of SplitCard

  and Spec = {
      Name : string
      ManaCost : ManaSymbol list
      Color : Color
      ColorIdent : Color option
      CardType : Set<CardType>
      Supertype : Set<Supertype>
      Subtype : Set<string>
          // Subtype の型は正確には「Set<ArtifactType> * Set<CreatureType> * ...」だが、面倒くさい。
      RuleText : string
      PowTou : (int * int) option
      Loyalty : int option
    }

  and SingleCardSpec = {
      Card : Card
      Expansions : Map<Expansion, (Rarity * flavor_t)>
    }

  and flavor_t = string

// 動的表現、つまりゲーム中のものを表現する型
// 使い道なし
(*
module Dynamic =
  open Core

  /// "sacrifice a creature", "tap an artifact you control", or something
  type Action =
      string

  // 能力
  type Cost =
      ManaCost * Set<PhaseSymbol> * (Action list)

  type Cond = 
    | TriggerAt of string // BeginningOfUpkeep
    | TriggerWhen of string

  type Ability =
    | Activated of Cost * string
    | Triggered of Cond * string
    | Static of string
    | Spell of string

  // カード
  type SplitCard = CardSpec * CardSpec

  /// represents a "physical" card
  and Card =
    | RegularCard     of CardSpec
    | FlipCard        of CardSpec * CardSpec
        
    /// FrontFace, BackFace
    | DoubleFacedCard of CardSpec * CardSpec
        
    /// LeftHalf, RightHalf
    | SplitCard       of SplitCard

  and Spec =
    {
      Name : string
      ManaCost : unit//Bag<Mana>
      Color : Color
      ColorIdentity : Color
      CardType : Set<CardType>
      Supertype : Set<Supertype>
      Subtype : Map<Supertype, Set<string>>
          // Subtype の型は正確には「Set<ArtifactType> * Set<CreatureType> * ...」だが、
          // それを型として記述するのは難しい。
      RuleText : string
      Ability : unit // Bag<Ability>
      Power : int
      Toughness : int
      Loyalty : int
    }

  and CardSpec = {
      Spec : Spec
      Flavor : string
      ExpansionSymbol : string * string
    }

  type AbilObj = {
      Source : Object
      Original : Ability
    }

  /// represents an object (in MTG rule)
  and Object =
    | CardObj of Card
    | Token of CardSpec
    | Copy of Object
    | AbilObj of AbilObj
    | FusedSplitSpell of SplitCard
    
  type ObjectInstance = {
      Position : string//Area
      Object : Object
    }
  ()
//*)

/// string expression
[<AutoOpen>]
module StrExp =
  /// language tag
  type Lang =
    /// en_US (default)
    | En
    /// ja_JP
    | Ja

  type ColorAtom with
      static member Chars =
          ColorAtom.Cases |> List.map char

      static member FromChar(c) =
          ColorAtom.Cases
          |> List.tryFind (fun ca -> (char ca) = c)

  type CardType with
      static member FromChar(c) =
          CardType.Cases
          |> List.tryFind (fun ct -> (char ct) = c)

  [<AutoOpen>]
  module Japanese =
    type ColorAtom with
        static member JaChars =
            [ '白'; '青'; '黒'; '赤'; '緑' ]

        static member FromJaChar (jaChar) =
            (ColorAtom.JaChars, ColorAtom.Cases) |> List.tryAssocUnzip jaChar

    type Supertype with
        static member JaNames =
            [ "伝説の"; "基本"; "氷雪"; "ワールド" ]

        static member FromJaName (jaName) =
            (Supertype.JaNames, Supertype.Cases) |> List.tryAssocUnzip jaName

    type CardType with
        static member JaNames =
            [ "アーティファクト"
              "クリーチャー"
              "エンチャント"
              "プレインズウォーカー"
              "土地"
              "インスタント"
              "ソーサリー"
              "部族"
            ]

        static member FromJaName (jaName) =
            (CardType.JaNames, CardType.Cases) |> List.tryAssocUnzip jaName
