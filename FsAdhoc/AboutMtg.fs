namespace AboutMtg

open System
open System.Text
open Basis.Core
open Uedai.Utilities

open System.Net

// 2014/03/17 現在のルール
module MTG =
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

    let allColorAtoms =
        [ White; Blue; Black; Red; Green ]

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

    type CardType =
      | Artifact
      | Creature
      | Enchantment
      | Land
      | Planeswalker
      | Instant
      | Sorcery
      | Tribal

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
          | ColorManaSymbol ca -> ColorPayable.Atom ca
          | HybridManaSymbol (lhs, rhs) -> ColorPayable.Or [lhs.ColorPayable; rhs.ColorPayable]
          | HalfManaSymbol src -> src.ColorPayable
          | NumManaSymbol _ | VarManaSymbol _
          | SnowManaSymbol | TwoLifeSymbol -> ColorPayable.Colorless
      member this.Color = this.ColorPayable.ToColor()

    type ManaCost = ManaSymbol list //Bag<ManaSymbol>
    let colorFromManacost (mc : ManaCost) =
        mc |> List.fold (fun clr sym -> clr + sym.Color) colorless

    type PhaseSymbol =
      /// {T}
      | TapSymbol
      /// {Q}
      | UntapSymbol

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
    open Core
    open Microsoft.FSharp.Reflection

    type DUStr<'T>() =
      static member val private Cases =
          FSharpType.GetUnionCases typeof<'T>

      static member val Names =
          DUStr<'T>.Cases
          |> Array.map (fun (case : UnionCaseInfo) -> case.Name)

      static member FromString str =
          let caseOpt =
              DUStr<'T>.Cases
              |> Array.tryFind (fun case -> case.Name = str)
          match caseOpt with
          | Some case -> FSharpValue.MakeUnion (case, [||])
          | None -> failwith ("unknown case of " + typeof<'T>.Name)
        
    type ColorAtom with
        static member FromChar = function
            | 'W' -> Some White
            | 'U' -> Some Blue
            | 'B' -> Some Black
            | 'R' -> Some Red
            | 'G' -> Some Green
            | _ -> None
            
        member this.ToChar =
            match this with
            | White -> 'W'
            | Blue  -> 'U'
            | Black -> 'B'
            | Red   -> 'R'
            | Green -> 'G'

    type CardType with
        static member FromChar = function
            | 'A' -> Artifact
            | 'C' -> Creature
            | 'E' -> Enchantment
            | 'L' -> Land
            | 'P' -> Planeswalker
            | 'I' -> Instant
            | 'S' -> Sorcery
            | 'T' -> Tribal
            | _ -> failwith "unknown cardtype"
        member this.ToChar =
            match this with
            | Artifact     -> 'A'
            | Creature     -> 'C'
            | Enchantment  -> 'E'
            | Land         -> 'L'
            | Planeswalker -> 'P'
            | Instant      -> 'I'
            | Sorcery      -> 'S'
            | Tribal       -> 'T'

    module Japanese =
      let jpColorAtoms = [ '白'; '青'; '黒'; '赤'; '緑' ]
      let colorAtomFromJp = function
          | '白' -> White
          | '青' -> Blue
          | '黒' -> Black
          | '赤' -> Red
          | '緑' -> Green
          | _ -> failwith "unknown color-atom"

      let jpSupertypes =
          [ "伝説の"; "基本"; "氷雪"; "ワールド" ]
      let supertypeFromJp = function
          | "伝説の"   -> Legendary
          | "基本"     -> Basic
          | "氷雪"     -> Snow
          | "ワールド" -> World
          | _ -> failwith "unknown supertype"

      let jpCardTypes =
          [ "アーティファクト"
            "クリーチャー"
            "エンチャント"
            "プレインズウォーカー"
            "土地"
            "インスタント"
            "ソーサリー"
            "部族"
          ]
      let cardTypeFromJp = function
          | "アーティファクト"     -> Artifact
          | "クリーチャー"         -> Creature
          | "エンチャント"         -> Enchantment
          | "プレインズウォーカー" -> Planeswalker
          | "土地"                 -> Land
          | "インスタント"         -> Instant
          | "ソーサリー"           -> Sorcery
          | "部族"                 -> Tribal
          | _ -> failwith "unknown cardtype"

module Scripts =
  open MTG.Core
  open MTG.Static

  module OriginalCards =
    open FParsec
    open MTG.StrExp
    open MTG.StrExp.Japanese

    type CardName = { Jp : string; En : string }
    with
        static member OfString s =
            let jp, en = s |> Str.split2 "/"
            { CardName.Jp = jp; En = en }
        override this.ToString() =
            this.Jp + (match this.En with "" -> "" | en -> "/" + en)

    let colorFromManacostOrIdentity (manacost, ciOpt) =
        ciOpt |> Option.getOr' (lazy colorFromManacost manacost)

    // 文法定義
    module CardSyntaxParser =
        type Parser<'T> = Parser<'T, unit>  // 「値制限」対策

        /// 改行を除く任意の文字列
        let anyInLine : Parser<_> = manyChars (noneOf "\n")

        /// 改行を除く空白
        let ws = skipMany (anyOf " \t")

        /// カード名
        let cardName =
            let content =
                    (attempt anyInLine |>> (fun jp -> { Jp = jp; En = "" }))
                <|> (pipe3 anyInLine (pchar '/') anyInLine
                        (fun jp slash en -> { Jp = jp; En = en }))

            between (pchar '《') (pchar '》') content

        let colorAtomJpChar =
            anyOf "白青黒赤緑" |>> colorAtomFromJp
        let colorAtomEnChar =
            anyOf "WUBRG" |>> (ColorAtom.FromChar >> Option.get)
        let colorAtomChar =
            (attempt colorAtomJpChar) <|> colorAtomEnChar

        /// マナ・コスト
        let manaSymbolJp : Parser<_> =
            let hybridManaSymbol lhs rhs =
                ManaSymbol.HybridManaSymbol (lhs, rhs)

            let rec groupSymbol =
                lazy( between (pchar '(') (pchar ')') (symbol : Lazy<_>).Value )

            and atomicSymbol =
                lazy(
                    (puint32
                        |>> NumManaSymbol)
                <|> (anyOf jpColorAtoms
                        |>> (colorAtomFromJp >> ColorManaSymbol))
                <|> (charReturn 'Φ' TwoLifeSymbol)
                <|> (charReturn '氷' SnowManaSymbol)
                <|> (CharParsers.letter
                        |>> (string >> VarManaSymbol))
                //<|> groupSymbol.Value
                )

            and symbol =
                lazy(
                    attempt (atomicSymbol.Value .>> (pstring "/2") |>> HalfManaSymbol)
                <|> attempt
                      (pipe2
                        (atomicSymbol.Value .>> (pchar '/'))
                        (sepBy1 atomicSymbol.Value (pchar '/'))   // 白/青/黒 のような並列を認める
                        (fun h t -> List.fold hybridManaSymbol h t)
                      )
                <|> atomicSymbol.Value
                )

            groupSymbol.Value

        let manaCostJp =
            many manaSymbolJp

        /// 色識別子
        let colorIdent =
            between (pchar '[') (pchar ']')
                (sepBy colorAtomChar (pchar '/'))

        let supertypesJp =
            sepBy (anyInLine |>> supertypeFromJp) (opt (pchar '・'))

        let cardTypesJp =
            sepBy1 (anyInLine |>> cardTypeFromJp) (opt (pchar '・'))

        (*
        let subtypeWithEnglish =
            between (pchar '(') (pchar ')') anyInLine
                
        let subtypeJp =
                (attempt (anyInLine .<< subtypeWithEnglish))
            <|> anyInLine
                
        let subtypesJp =
            sepBy subtypeJp (attempt (pchar '・') <|> ws)
        //*)
        let subtypeJp =
            regex @"[^\n()]+(\(.+\))?"
        let subtypesJp =
            sepBy subtypeJp ((skipChar '・') <|> ws)
                
        /// タイプ行
        let typeline : Parser<_> =
            (pipe4
                ((opt colorIdent) .>> ws)
                supertypesJp
                (cardTypesJp .>> ws)
                (((many1 (anyOf "―-－～~")) >>. ws) >>. (subtypesJp .>> ws))
                (fun ci supertypes cardtypes subtypes ->
                    (ci, supertypes, cardtypes, subtypes)
                ))

        /// P/T、loyalty
        /// */*+1 とかは未定義とみなす
        let powTou : Parser<_> =
            (pipe2 (pint32 .>> (pchar '/')) pint32
                (fun p t -> (Some (p, t), None)))
        let loyalty : Parser<_> =
            (pstring "loyalty: ") >>. pint32
                |>> (fun l -> (None, Some l))

        /// FT
        let flavorText =
            (pstring "FT：" <|> pstring "FT:") >>. ws >>. (manyChars (noneOf "#"))

        /// コメント
        let commentline : Parser<_> =
            regex @"\n#[^\n]*"

        /// カード
        let anyStringWithoutEmptyLine : Parser<_> =
            manyCharsTill anyChar (attempt (pstring @"\n\n"))

        let parseCardSpec : Parser<_> =
            pipe5
                (cardName .>> ws)
                (manaCostJp .>> (ws .>> newline))
                (typeline .>> newline) 
                (anyStringWithoutEmptyLine .>> newline)
                ((attempt powTou <|> loyalty) .>> newline)
                (fun name manacost (colorIdent, supertypes, cardtypes, subtypes) text (pt, loyalty) ->
                    let colorIdent = colorIdent |> Option.map colorFromAtoms
                    {
                        Name       = name.ToString()
                        ManaCost   = manacost
                        Color      = colorFromManacostOrIdentity (manacost, colorIdent)
                        ColorIdent = colorIdent
                        Supertype  = Set.ofList supertypes
                        CardType   = Set.ofList cardtypes
                        Subtype    = Set.ofList subtypes
                        RuleText   = text
                        PowTou     = pt
                        Loyalty    = loyalty
                    }
                )

        // regular, flip, double-faced, splits
        let parseRegularCard : Parser<_> =
            parseCardSpec |>> RegularCard

        let parseFlipOrDoubleFacedCard : Parser<_> =
            pipe3
                parseCardSpec
                ((stringReturn "<flip>" FlipCard) <|> stringReturn "<double-faced>" DoubleFacedCard)
                parseCardSpec
                (fun c1 ctor c2 -> ctor (c1, c2))

        let parseSplitCard : Parser<_> =
            sepBy1 parseCardSpec (skipString "<split>")
            |>> SplitCard

        let parseCardBody =
            (attempt parseRegularCard)
            <|> (attempt parseFlipOrDoubleFacedCard)
            <|> parseSplitCard

        let parseCard : Parser<_> =
            pipe3 parseCardBody (opt flavorText) (many commentline) (fun card ft comms ->
                {
                    Card = card
                    Expansions = Map.empty // TODO: analyze comms and make this Map
                })

  module SpoilerProc =
    open System.IO
    open System.Text.RegularExpressions
    open System.Xml.Linq

    /// カード名辞書 (「和名/英名」の行区切り文章ファイル) を読み込む
    let loadCardnameDictionary pathDic =
        use file = File.OpenText pathDic
        [
            while not file.EndOfStream do
                yield (file.ReadLine() |> Str.split2 "/")
        ] |> Map.ofList

    /// スポイラーテキストのカード名を翻訳する
    let spoilerCardnameTranslate (dict : Map<string, string>) spoiler =
        Regex.Replace (spoiler, "Card Name:\t(.*?)\r\n", fun (mch : Match) ->
            "Card Name:\t" + dict.[mch.Groups.[1].Value] + "\r\n"
        )
    /// スポイラーテキストのタイプ行を翻訳する
    let spoilerTypeTranslate =
        // 並び順については未実装 (例：伝説のクリーチャー・エンチャント/Legendary Enchantment Creature)
        let dictTypes =
            [
                ("伝説の", "Legendary ")
                ("基本", "Basic ")
                ("氷雪", "Snow ")
                ("ワールド", "World")

                ("部族", "Tribal")
                ("土地", "Land")
                ("クリーチャー", "Creature")
                ("エンチャント", "Enchantment")
                ("アーティファクト", "Artifact")
                ("プレインズウォーカー", "Planeswalker")
                ("インスタント", "Instant")
                ("ソーサリー", "Sorcery")
            ] |> Map.ofList
        (fun spoiler ->
            Regex.Replace (spoiler, "Type & Class:\t(.*?)( - .*)?\r\n", fun (mch : Match) ->
                let types = mch.Groups.[1].Value
                let types =
                    dictTypes |> Map.fold (fun types key value ->
                            types |> Str.replace key value
                    ) types
                    |> Str.replace "・" " "
                "Type & Class:\t" + types + mch.Groups.[2].Value + "\r\n"
            )
        )

    let spoilerProc () =
        let dict = loadCardnameDictionary "D:/dic.txt"
        let spoiler =
            File.ReadAllText("D:/spoiler.txt")
            |> spoilerCardnameTranslate dict
            |> spoilerTypeTranslate
        File.WriteAllText ("D:/spoiler_transed.txt", spoiler)

    /// 画像ファイルの名前を変更する
    (*
        1. 公式 Visual Spoiler のHTMLを解析して、画像ファイルと日本語カード名の対応を得る。
        それをタブ文字区切りにして $PicPathMap.txt として UTF-8 で保存する。
        2. どこか(今回は Whisper のカードデータベース)から、日本語カード名と英語カード名の対応を得る。
        公式の英語版から、同じ順序の英語名リストを得てもいいかもしれない (スクリプトを変える必要があるが)。
        その対応を、/ 区切りで $NameDic.txt として UTF-8 で保存する。
        3. IE で公式 Visual Spoiler を開き、すべての画像のキャッシュを得る。
        IE のキャッシュフォルダ (defaultにリンクがある; Temporary Internet Archive / Low / Content.IE5) から
        だいたいすべての画像ファイルを複製して、1つのフォルダに入れる。
        先のテキストファイル2つもこのフォルダに入れる。フォルダ名はセット略称にするとよい。
        4. そのフォルダへのファイルパスを指定して、この関数を実行する。
        5. 最後に、不要なファイルをすべて除去する (更新日時を使う)。
    *)
    let renamePics dir =
        // 日本語カード名 → 画像ファイルの名前、の対応表 (タブ区切り)
        let fileNameMap = "$PicPathMap.txt"

        // 日本語カード名 → 英語カード名、の対応表 (/ 区切り)
        let fileNameDic = "$NameDic.txt"

        let dic = loadCardnameDictionary (dir + "/" + fileNameDic)
        let picNameMap =
            File.ReadLines (dir + "/" + fileNameMap)
            |> Seq.map (Str.split2 "\t")
            |> Map.ofSeq
                
        for kv in picNameMap do
            let jp, path = kv.Key, kv.Value
            let en = dic.TryFind jp

            match en with
            | Some en ->
                try
                    File.Move (dir + "/" + path, dir + "/" + en + ".jpg")
                with
                | :? IO.FileNotFoundException ->
                    printfn "%s's Pic File can't be found." jp
            | None ->
                printfn "%s's English name can't be found." jp

        ()

    type SpoilerCardItem = {
        Name : string
        Color : string
        Cost : string
        TypeClass : string * string option
        PowTou :(string * string) option
        Text : string
        Flavor: string
        Artist : string
        Rarity : string
        Number : int option
    }
    let descriptionToSpoilerCardItem =
        let attrMap =
            [
            //    "Name: ", "Card Name"
                "Cost: ", "Mana Cost"
                "Type: ", "Type & Class"
                "Rules Text: ", "Card Text"
                "Flavor Text: ", "Flavor Text"
                "Rarity: ", "Rarity"
                "Pow/Tgh: ", "Pow/Tou"
                "Illus. ", "Artist"
                "Set Number: #", "Card #"
            ]
            |> List.map (fun (k, v) -> (new Regex(Regex.Escape(k) + "(.*?)<br>"), v))
        let f desc =
            attrMap |> List.choose (fun (reg, v) ->
                let m = reg.Match (desc)
                Option.if' (m.Success) (fun () ->
                    v + ":\t" + m.Groups.[1].Value
                )
            ) |> Str.join "\n"
        f
    /// MTG Salvation の spoiler.rss からスポイラーテキストを生成する
    // → P/T 抜けなどが多いので却下
    let spoilerTextFromRss dir =
        let fileNameRss = "$SpoilerRss.txt"
        let rss = XDocument.Load(dir + "/" + fileNameRss)
        let items = rss.Element(XName.Get "rss").Element(XName.Get "channel").Elements(XName.Get "item")
        let spoilerItems =
            [ for item in items do
                let name = item.Element(XName.Get "title").Value
                let description = item.Element(XName.Get "description").Value
                let setName = (Regex.Match (description, "Set: (.*?)<br>")).Groups.[1].Value
                if setName = "Journey Into Nyx" then
                    yield (
                        "Card Name:\t" + name + "\n"
                        + (descriptionToSpoilerCardItem description)
                    )
            ]
        let spoiler =
            spoilerItems |> Str.join "\n"
        File.WriteAllText (dir + "/$Spoiler.txt", spoiler)

    ///テキストスポイラーの読み込み
    let parseTextSpoiler text =
        let regPattern = """
Card Name:	(.*?)
Card Color:	(.*?)
Mana Cost:	(.*?)
Type & Class:	(.*?)(?: - (.*?))?
Pow/Tou:	(?:((?:\d|\*)*?)/((?:\d|\*)*?))?
Card Text:	((?:.|\r\n)*?)
Flavor Text:	((?:.|\r\n)*?)
Artist:		(.*?)
Rarity:		(.*?)
Card #:		(.*?)/(.*?)
"""
        let reg = new Regex(regPattern)
        let cntCards = ref None
        [ for m in reg.Matches(text) do
            let typeclass =
                let gType = m.Groups.[4]
                let gClass = m.Groups.[5]
                gType.Value, (Option.if' gClass.Success (fun() -> gClass.Value))
            let powtou =
                let gPow = m.Groups.[6]
                let gTou = m.Groups.[7]
                Option.if' (gPow.Success && gTou.Success) (fun() ->
                    gPow.Value, gTou.Value
                )
            let cardNumber = m.Groups.[12].Value |> Int32.TryParse |> Option.trialResult
            let countCardsTotal =
                m.Groups.[13].Value |> Int32.TryParse |> Option.trialResult
            if !cntCards <> countCardsTotal then
                if cntCards.Value.IsSome then
                    printfn "warning: count of card total may be wrong at Card#%A" cardNumber
                cntCards := countCardsTotal

            let x =
                {
                  Name      = m.Groups.[1].Value
                  Color     = m.Groups.[2].Value
                  Cost      = m.Groups.[3].Value
                  TypeClass = typeclass
                  PowTou    = powtou
                  Text      = m.Groups.[8].Value |> Str.remove "\t"
                  Flavor    = m.Groups.[9].Value |> Str.remove "\t"
                  Artist    = m.Groups.[10].Value
                  Rarity    = m.Groups.[11].Value
                  Number    = cardNumber
                }
            yield x
        ]

    ///mws式マナコスト文字列から色とCMCを分析
    let colorsAndCMCFromManacostInMWS manacost =
        //todo:やや汎用的な構文処理パターンが溶けているのでまとめたい
        let patternlist =
            [
              "multicolored_hybrid", @"(?<h2c1>[WUBRG])/(?<h2c2>[WUBRG])"
              "monocolored_hybrid" , @"2/(?<h1c>[WUBRG])"
              "phyrexian"          , @"(?<phyc>[WUBRG])/P"
              "monocolored"        , @"[WUBRG]"
              "generic"            , @"[1-9]\d*"
              "variable"           , @"[XYZ]"
              "snow"               , @"S"
            ]

        let pattern =
            patternlist
            |> List.map (fun (capture_name, pattern) ->
                "(?<" + capture_name + ">" + pattern + ")"
                )
            |> Str.join "|"

        let matches =
            Regex.Matches(manacost, pattern, RegexOptions.ExplicitCapture)

        let (|MatchNamed|_|) (capture_name: string) (match': Match) =
            let group' = match'.Groups.[capture_name]
            Option.if' group'.Success (fun() -> group')

        let colorlist, cmclist = //ゴミの混ざったリストになる
            [ for m in matches do
                match m with
                | MatchNamed "multicolored_hybrid" g ->
                    yield m.Groups.["h2c1"].Value, 1
                    yield m.Groups.["h2c2"].Value, 0
                | MatchNamed "monocolored_hybrid" g ->
                    yield m.Groups.["h1c"].Value, 1
                | MatchNamed "phyrexian" g ->
                    yield m.Groups.["phyc"].Value, 1
                | MatchNamed "monocolored"g ->
                    yield g.Value, 1
                | MatchNamed "generic" g ->
                    let success, n = g.Value |> Int32.TryParse
                    assert success
                    yield "", n
                | MatchNamed "variable" g ->
                    () // no color and zero cmc
                | MatchNamed "snow" g ->
                    yield "", 1
                | _ -> failwith "unknown"
            ]
            |> List.unzip

        let colorlist' = //regularize
            [ for c in "WUBRG" do
                if colorlist |> Str.join "" |> Seq.contains c then yield c |> string ]
        let cmc = cmclist |> List.sum

        (colorlist', cmc)

    ///プレイしたときに通常どの領域に置かれるかを表す数値
    ///0: Land row; 1: Art/Ench/PW row; 2: Creature row; 3: Stack
    let tableRowFromType =
        let (|Contains|_|) self src =
            if self |> Str.contains src then Some () else None
        function
        | Contains "Land" -> 0
        | Contains "Creature" -> 2
        | Contains "Instant"
        | Contains "Sorcery" -> 3
        | _ -> 1

    ///text spoilerからCockatrice XML形式に変換する
    let spoilerCockatriceXmlFromText setShortName text =
        let cards = parseTextSpoiler text
        let s = new StringWriter()
        for card in cards do
            let colors, cmc =
                colorsAndCMCFromManacostInMWS (card.Cost)
            let colortags =
                colors
                |> List.map (fun x -> "<color>" + x + "</color>")
                |> Str.join ""
            let typeline =
                match card.TypeClass with
                | (typ, Some cls) -> typ + " - " + (cls |> Str.trim)
                | (typ, None) -> typ
            let tablerow =
                tableRowFromType (card.TypeClass |> fst)
            let pt =
                card.PowTou
                |> Option.map (fun (p, t) -> " <pt>" + p + "/" + t + "</pt>")
                |> Option.getOr ""
            let text' =
                //todo:テキスト中のマナシンボル表現を { } で括る
                //英単語とうまく区別するのは難しい
                card.Text
            let ft' =
                //flavortextタグがあるとダメっぽい
                //if card.Flavor.Length > 0 then " <flavortext>" + card.Flavor + "</flavortext>" else ""
                ""

            let result =
                "<card> <name>" + card.Name + "</name>"
                + " <set muId=\"3\">" + setShortName + "</set>"
                    // muIdは必須だが画像があれば無視される、とりあえずBlack Lotusにしておく
                + colortags
                + " <manacost>" + card.Cost + "</manacost>"
                + " <cmc>" + (cmc |> string) +  "</cmc>"
                + " <type>" + typeline +  "</type>"
                + " <tablerow>" + (tablerow |> string) + "</tablerow>"
                + pt
                + " <text>" + text' + "</text>"
                + ft'
                + " </card>\r" //改行は \r がいいっぽい
            s.Write(result)

        s.ToString()

    ///日本語カード画像リストをGathererの検索結果からダウンロードする
    ///検索結果のhtmlは手動でとっておく
    let downloadJapaneseCardImagesFromGatherer dir htmltext =
        if not <| Directory.Exists dir then
            Directory.CreateDirectory dir |> ignore

        use webcl = new WebClient()
        let matches =
            let p =
                """<a .*?href=".*?multiverseid=(\d+)"><img .*?alt="(.+?) \((.+?)\)" />"""
            Regex.Matches(htmltext, p)
        let counter = ref 1
        for m in matches do
            let muId    = m.Groups.[1].TryValue
            let jaName  = m.Groups.[2].TryValue
            let enName  = m.Groups.[3].TryValue
            match muId, jaName, enName with
            | Some muId, Some jaName, Some enName ->
                printfn "download...(%d/%d)《%s/%s》" (!counter) matches.Count jaName enName
                counter |> Ref.inc
                let urlImage = @"http://gatherer.wizards.com/Handlers/Image.ashx?multiverseid=" + (muId |> string) + "&type=card"
                let filePath = Path.Combine (dir, enName + ".jpg")
                webcl.DownloadFile (urlImage, filePath)
            | _ -> failwith "invalid html"
        ()

    ///Whisperのカードリストから和名/英名のリストを得る
    let downloadCardNameTranslatorForJaFromWhisper setShortName =
        use webcl = new WebClient()
        let urlSpoilerText =
            "http://whisper.wisdom-guild.net/cardlist/" + setShortName + ".txt"
        let spoilerText =
            webcl.DownloadString (urlSpoilerText)
        let pattern =
            """英語名：(.+?)\n日本語名：(.+?)（"""

        [ for mch in Regex.Matches(spoilerText, pattern) do
            assert (mch.Groups.[1].Success && mch.Groups.[2].Success)
            let enName = mch.Groups.[1].Value
            let jaName = mch.Groups.[2].Value
            yield (jaName, enName) ]
        |> Map.ofList

    ///カードギャラリーから日本語カード画像リストをダウンロードする
    ///tranaslator: 日本語カード名→英語カード名の関数
    let downloadJapaneseCardImagesFromCardGallery dir (htmltext: string) (translator: string -> string option) =
        if not <| Directory.Exists dir then
            Directory.CreateDirectory dir |> ignore

        let matches =
            Regex.Matches
              ( htmltext
              , """<img .*?src="(.*?)" /><br /><i>(.+?)</i>""")
        let counter = ref 1
        use webcl = new WebClient()
        for m in matches do
            match m.Groups.[1].TryValue, m.Groups.[2].TryValue with
            | Some urlImage, Some jaName ->
                match translator jaName with
                | Some enName -> 
                    printfn "download...(%d/%d)《%s/%s》"
                      (!counter)
                      matches.Count
                      jaName
                      enName
                    counter |> Ref.inc
                    let filePath = Path.Combine (dir, enName + ".png")
                    webcl.DownloadFile (urlImage, filePath)
                | None -> printfn "translation error: \"%s\"" jaName
            | _ -> failwith "invalid html"
        ()

    ///日本語カード画像リストを、日本語名とURLのリストを参考にダウンロードする
    ///ファイルは「和名\tURL」の改行区切り
    let downloadJapaneseCardImagesFromUrlList dir text (translator) =
        if not <| Directory.Exists dir then
            Directory.CreateDirectory dir |> ignore

        let counter = ref 1
        use webcl = new WebClient()
        let lines = text |> Str.splitBy "\r\n" |> Array.filter ((<>) "")
        for line in lines do
            let jaName, urlImage =
                line |> Str.split2 "\t"
            let enName =
                match translator jaName with
                | Some enName -> enName
                | _ -> printfn "translation error: \"%s\"" jaName; jaName
            printfn "download...(%d/%d)《%s/%s》" (!counter) lines.Length jaName enName
            counter |> Ref.inc
            let filePath = Path.Combine (dir, enName + (Path.GetExtension urlImage))
            webcl.DownloadFile (urlImage, filePath)
        ()

  let testAll () =
      let ``test for colorsAndCMCFromManacostInMWS`` =
          let rw4 = SpoilerProc.colorsAndCMCFromManacostInMWS "4RW"
          assert (rw4 = (["W"; "R"], 6))
          let hybrid5 = SpoilerProc.colorsAndCMCFromManacostInMWS "3R/GR/U"
          assert (hybrid5 = (["U"; "R"; "G"], 5))
          ()
      ()

  let renamePics () =
      let dir = @"D:\Game\MWS\Pics\JOU"
      dir |> SpoilerProc.renamePics
      dir |> SpoilerProc.spoilerTextFromRss

  let analyzeSpoiler () =
      (*
      let testFile = @"D:/Docs/downloads/_.txt"
      let text = File.ReadAllText testFile
      let parsed = SpoilerProc.parseTextSpoiler spoilerText
      let cockxml = SpoilerProc.spoilerCockatriceXmlFromText "DVO" spoilerText
      do
        SpoilerProc.downloadJapaneseCardImagesFromGatherer @"D:/Docs/downloads/M15" text
      //*)
      (*
      do  let setShortName = "FRF"
          let dic = SpoilerProc.downloadCardNameTranslatorForJaFromWhisper setShortName
          SpoilerProc.downloadJapaneseCardImagesFromCardGallery (Path.Combine (@"D:\Game\MWS\Pics", setShortName)) text (fun s -> dic.TryFind s)
      //*)
      (*
      do  let setShortName = "M15"
          let dic = SpoilerProc.downloadCardNameTranslatorForJaFromWhisper setShortName
          SpoilerProc.downloadJapaneseCardImagesFromUrlList (Path.Combine (@"D:\Game\MWS\Pics", setShortName)) text (fun s -> dic.TryFind s)
      //*)
      ()

  let mainMtg (argv: string []) =
      ()
