namespace MTG

open System
open System.Text
open Basis.Core
open Uedai.Utilities

open FParsec
open MTG.StrExp

module Handmade =
  type CardName =
    {
      En: string option
      Ja: string option
    }
  with
    static member Empty =
        { Ja = None; En = None }

    override this.ToString() =
        match (this.En, this.Ja) with
        | Some en, Some ja -> en + "/" + ja
        | enOpt, jaOpt ->
            (enOpt |> Option.getOr "") + (jaOpt |> Option.getOr "")

  let colorFromIdentityOrManaCost ciOpt manacost =
      ciOpt |> Option.getOr' (lazy colorFromManaCost manacost)

  [<AutoOpen>]
  module private Detail =
    let skip p =
        p |>> ignore

    let skipDotOrBlank =
        skipChar '・' <|> skip blankMany1

    let betweenParen l r =
        between (skipChar l) (skipChar r)

    /// Apply `choice ps`.
    /// If ps.[i] has succeeded and resulted in `x` then return ``fs.[i] x``.
    let lookupUnzip ps fs =
        assert ((ps |> Seq.length) = (fs |> Seq.length))
        (ps, fs)
        ||> Seq.map2 (|>>)
        |> choice

    let lookupStrMap strs vals =
        lookupUnzip (strs |> List.map skipString) (vals |> List.map konst)

    let inline choiceEnum<'T> =
        lookupStrMap
          Reflection.DU<'T>.Names
          Reflection.DU<'T>.UnitCases
          
    /// Same as ``sepBy p sep`` except for this never fails
    let safeSepBy p sep =
        opt (attempt (pipe2 p (many (attempt (sep >>. p))) List.cons))
        |>> Option.getOr []

    ///Unwrap each element in tuple of options if able
    let unwrapOption2 = function
      | (Some v0, Some v1) -> Some (v0, v1)
      | _ -> None

  // 文法定義
  module CardSyntaxParser =
    open Uedai.Utilities.FParsec

    let colorAtomCharFrom (chars) =
        let ps =
          (ColorAtom.Cases, chars)
          ||> List.map2 (fun ca c -> skipChar c >>% ca)
        choice ps

    let colorAtomChar: Parser<_> =
        (   colorAtomCharFrom (ColorAtom.Chars)
        <|> colorAtomCharFrom (ColorAtom.JaChars)
        )

    let manaSymbol =
        let minimal =
            (   (puint32          |>> ManaSymbol.Unspecified)
            <|> (colorAtomChar    |>> ManaSymbol.Monocolored)
            <|> (skipChar 'C'     >>% ManaSymbol.Colorless)
            <|> (anyOf "PΦ"      >>% ManaSymbol.TwoLife)
            <|> (anyOf "S氷"      >>% ManaSymbol.Snow)
            <|> (anyOf "XYZ"      |>> ManaSymbol.Var)
            )

        let hybridMany1 =
            pipe2
              minimal 
              (many (skipChar '/' >>. minimal))
              (List.fold (curry2 ManaSymbol.Hybrid))

        let symbol = hybridMany1

        ( attempt
            (betweenParen '{' '}' symbol)
        <|> (betweenParen '(' ')' symbol)
        )

    let manaCost: Parser<_> =
        many manaSymbol

    let colorIdent =
        betweenParen '[' ']'
          (sepBy colorAtomChar (skipChar '/'))

    let typeLineTmpl supertypeList cardtypeList subtypeList =
        let separator =
            skipMany1 (skipAnyOf  "-－―~～") >>. blankMany

        tuple4
          (opt colorIdent
            .>> blankMany)
          (supertypeList
            .>> optional skipDotOrBlank)
          (cardtypeList
            .>> blankMany)
          (opt
            (separator
            >>. subtypeList
            //TODO: rarity
            ) |>> Option.getOr [])
         .>> blankMany .>> skipNewline

    // '*' を含む式
    let starExpr =
        regex @"[0-9XYZ+() \t-]*\*[0-9XYZ+*() \t-]*"

    let intOrStarExpr =
        (   (attempt pint32  |>> Some)
        <|> (starExpr        >>% None)
        )

    /// P/T
    /// '*' の式は、認識するが理解しない
    let powTou =
        tuple2
          (intOrStarExpr .>> skipChar '/') intOrStarExpr
        |>> (unwrapOption2 >> Option.map PowTou)

    let loyalty =
        skipString "loyalty:" >>. blankMany
        >>. intOrStarExpr
        |>> Option.map Loyalty

    let powTouLoyalty =
        attempt powTou
        <|> loyalty

    /// コメント行
    /// 行頭でだけ使う
    let commentLine =
        skipChar '#' >>. restOfLine false

    let commentLines =
        sepBy commentLine skipNewline

    let skipNewlineAndCommentLines =
        skipNewline >>. skip commentLines

    let flavorTextKeyword =
        skipString "FT" >>. skipAnyOf ":："

    /// フレイバーテキスト
    /// 行頭でだけ使う
    let flavorTextLines =
        let forbidden =
            attempt skipEmptyLine

        flavorTextKeyword >>. blankMany
        >>. sepBy
              (notFollowedBy forbidden >>. restOfLine false)
              skipNewlineAndCommentLines
        |>> Str.join "\r\n"

    let ruleTextLines =
        let forbidden =
            attempt (blankMany >>. skip powTouLoyalty)
            <|> attempt flavorTextKeyword
            <|> skipEmptyLine

        let line =
            notFollowedBy forbidden
            >>. restOfLine false

        commentLines
        >>. sepBy line skipNewlineAndCommentLines
        |>> Str.join "\r\n"

    [<AutoOpen>]
    module En =
      let supertypeList =
          let p = lookupStrMap Supertype.Names Supertype.Cases
          safeSepBy p blankMany1 

      let cardTypeList =
          safeSepBy choiceEnum<CardType> blankMany1

      let subtype =
          many1Chars (letter <|> digit <|> anyOf "'-_")

      let subtypeList =
          sepBy subtype blankMany1

      let typeLine =
          typeLineTmpl supertypeList cardTypeList subtypeList

    module Ja =
      /// 日本語/英語
      let cardName =
          let enName =
              manyChars (noneOf "/》")
              |>> Option.ifSat (not << Str.isNullOrEmpty)

          let jaName =
              opt (
                skipChar '/'
                >>. manyChars (noneOf "》")
                )

          betweenParen '《' '》' 
            (pipe2 enName jaName (fun en ja -> { En = en; Ja =  ja }))

      let supertypeList =
          safeSepBy
            (lookupStrMap Supertype.JaNames Supertype.Cases)
            (optional skipDotOrBlank)

      let cardTypeList =
          safeSepBy
            (lookupStrMap CardType.JaNames CardType.Cases)
            (optional skipDotOrBlank)

      /// 英名併記を許可
      let subtype =
          many1Chars letter
          .>> optional (betweenParen '(' ')' En.subtype)

      let subtypeList =
          sepBy subtype skipDotOrBlank

      let typeLine =
          typeLineTmpl supertypeList cardTypeList subtypeList

    /// カード
    let cardSpec: Parser<_> = parse {
        let nl = skipNewlineAndCommentLines
        let typeLineParser =
            attempt En.typeLine <|> Ja.typeLine

        let! name     = Ja.cardName      .>> blankMany .>> optional (attempt nl)
        let! manaCost = manaCost         .>> blankMany .>> nl
        let! typeLine = typeLineParser
        let! ruleText = ruleTextLines    .>> blankMany
        let! ptl      = powTouLoyalty
        do! if ptl.IsSome then blankMany >>. nl else preturn ()

        let (colorIdent, supertypes, cardtypes, subtypes) = typeLine
        let colorIdent = colorIdent |> Option.map colorFromAtoms
        return {
            Name       = string name
            ManaCost   = manaCost
            Color      = colorFromIdentityOrManaCost colorIdent manaCost
            ColorIdent = colorIdent
            Supertype  = Set.ofList supertypes
            CardType   = Set.ofList cardtypes
            Subtype    = Set.ofList subtypes
            RuleText   = ruleText
            PowTou     = (match ptl with | Some (PowTou pt) -> Some pt | _ -> None)
            Loyalty    = (match ptl with | Some (Loyalty l) -> Some l  | _ -> None)
          }
      }

    let regularCard: Parser<_> =
        cardSpec |>> RegularCard

    let flipOrDoubleFacedCard: Parser<_> =
        let connector =
                (stringReturn "<flip>" FlipCard)
            <|> (stringReturn "<double-faced>" DoubleFacedCard)
        pipe3
          cardSpec connector cardSpec
          (fun c1 ctor c2 -> ctor (c1, c2))

    let splitCard: Parser<_> =
        sepBy1 cardSpec (skipString "<split>")
        |>> SplitCard

    let cardBody =
        (attempt regularCard)
        <|> (attempt flipOrDoubleFacedCard)
        <|> splitCard

    let cardSingle = parse {
        let! card = cardBody
        let! ft   = opt flavorTextLines
        // TODO: analyze comments and build expansions
        return SingleCardSpec.ofCard(card)
      }

    let skipBlankLines1 =
        skipMany1
          (skipNewline
            >>. (skip commentLines <|> (blankMany >>. followedByNewline)))

    let cardList =
        sepBy cardSingle skipBlankLines1

    let cardListFile =
        blankMany
        >>. optional skipBlankLines1
        >>. cardList
        .>> optional skipBlankLines1
        .>> eof

    let test () =
        let allSuccess p cases =
            cases
            |> List.iter (fun (expr, xp) ->
              let result = run p expr
              match result with
              | ParserResult.Success (value, _, _) ->
                  if xp <> value
                  then failwithf "WA\r\nexpected = %A\r\nbut result is %A" xp value
              | ParserResult.Failure (message, _, _) ->
                  failwithf "Failed parsing:\r\ninput = %s\r\n%s" expr message
              )

        do
            let (w, u, b, r, g) =
                ( ManaSymbol.Monocolored White
                , ManaSymbol.Monocolored Blue
                , ManaSymbol.Monocolored Black
                , ManaSymbol.Monocolored Red
                , ManaSymbol.Monocolored Green
                )
            let p = ManaSymbol.TwoLife
            let num = ManaSymbol.Unspecified
            [ ""                    , []
              "{1}"                 , [num 1u]
              "{15}"                , [num 15u]
              "{W}{U}{B}{R}{G}"     , [w; u; b; r; g]
              "(白)(青)(黒)(赤)(緑)", [w; u; b; r; g]
              "{W}{W}{U}"           , [w; w; u]
              "{X}"                 , [ManaSymbol.Var 'X']
              "{C}"                 , [ManaSymbol.Colorless]
              "{S}"                 , [ManaSymbol.Snow]
              "{U/P}"               , [u/p]
              "{W/U}"               , [w/u]
              "{2/W}"               , [(num 2u)/w]
              "{2/W/U}"             , [(num 2u)/w/u]
            ] |> allSuccess manaCost

        do
            [ "[]"       , []
              "[W]"      , [White]
              "[W/U]"    , [White; Blue]
              "[W/W/U]"  , [White; White; Blue]
            ] |> allSuccess colorIdent

        do
            [ "Legendary Creature"      , [Legendary]
              "World Basic Land"        , [World; Basic]
            ] |> allSuccess (En.supertypeList)

            [ "伝説の"                  , [Legendary]
              "ワールド・基本土地"      , [World; Basic]
              "伝説の氷雪・部族・土地"  , [Legendary; Supertype.Snow]
            ] |> allSuccess (Ja.supertypeList)

            [ "Creature"                , [Creature]
              "Artifact Land"           , [Artifact; Land]
            ] |> allSuccess (En.cardTypeList)

            [ "クリーチャー"            , [Creature]
              "土地・エンチャント"      , [Land; Enchantment]
              "部族アーティファクト"    , [Tribal; Artifact]
            ] |> allSuccess (Ja.cardTypeList)

            [ "インスタント"
                , (None, [], [Instant], [])
              "プレインズウォーカー ― ティボルト(Tibolt)"
                , (None, [], [Planeswalker], ["ティボルト"])
              "伝説の氷雪クリーチャーエンチャント ― 人間・戦士"
                , ( None, [Legendary; Supertype.Snow]
                  , [Creature; Enchantment], ["人間"; "戦士"])
              "[白/青] アーティファクト ― 装備品(Equipment)"
                , (Some[White; Blue], [], [Artifact], ["装備品"])
            ]
            |> List.map (fun (expr, r) -> (expr + "\r\n", r)) 
            |> allSuccess (Ja.typeLine)
            
            [ "Instant"
                , (None, [], [Instant], [])
              "Planeswalker -- Tibolt"
                , (None, [], [Planeswalker], ["Tibolt"])
              "Legendary Snow Enchantment Creature -- Human Warrior"
                , ( None, [Legendary; Supertype.Snow]
                  , [Enchantment; Creature], ["Human"; "Warrior"])
              "[W/U] Artifact~Equipment"
                , (Some[White; Blue], [], [Artifact], ["Equipment"])
            ]
            |> List.map (fun (expr, r) -> (expr + "\r\n", r))
            |> allSuccess (En.typeLine)

            [ """《灰色熊/Grizzly Bear》 (1)(緑)
                  クリーチャー ― 熊(Bear)
                  2/2
              """
              , let mc = [ManaSymbol.Unspecified 1u; ManaSymbol.Monocolored Green] in
                  RegularCard (Spec.ofCreature "灰色熊/Grizzly Bear" mc ["熊"] "" (2, 2))
            ] |> allSuccess (cardBody)


        ()

  open System.IO
  let main () =
      CardSyntaxParser.test ()
      let text = File.ReadAllText (@"D:\Docs\archive\_nobak\__testdata.txt")
      let result =
          let p = CardSyntaxParser.cardListFile
          run p text
      match result with
      | ParserResult.Success (value, stat, pos) ->
          ()
      | ParserResult.Failure (message, err, stat) ->
          ()
      ()
