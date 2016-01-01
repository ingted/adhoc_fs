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

  let colorFromManacostOrIdentity (manacost, ciOpt) =
      ciOpt |> Option.getOr' (lazy colorFromManacost manacost)

  [<AutoOpen>]
  module private Detail =
    let skip p =
        p |>> ignore

    let skipDotOrBlank =
        skipChar '・' <|> skip blankMany1

    let betweenParen l r =
        between (skipChar l) (skipChar r)

    ///Unwrap each elements in tuple of options if able
    let unwrapOption2 = function
      | (Some v0, Some v1) -> Some (v0, v1)
      | _ -> None

  // 文法定義
  module CardSyntaxParser =
    open Uedai.Utilities.FParsec

    /// カード名
    /// 日本語/英語
    let cardName =
        let content =
            (   attempt anyInLine
                |>> (fun ja -> { CardName.Empty with Ja = Some ja })
            <|> pipe3
                  anyInLine (skipChar '/') anyInLine
                  (fun ja slash en -> { Ja = Some ja; En = Some en })
            )

        betweenParen '《' '》' content

    let colorAtomCharFrom (charset: SymbolCharSet) =
        let ps =
          (ColorAtom.Cases, charset.ColorAtoms)
          ||> List.map2 (fun ca c -> skipChar c >>% ca)
        choice ps

    let colorAtomChar: Parser<_> =
        colorAtomCharFrom symbolCharSet
        <|> colorAtomCharFrom SymbolCharSet.Ja

    /// マナ・シンボル
    let manaSymbolFrom (charset: SymbolCharSet) =
        let symbol, symbolRef =
            FParsec.Primitives.createParserForwardedToRef ()

        let groupSymbol =
            ( attempt
                (between (pchar '{') (pchar '}') symbol)
            <|> (between (pchar '(') (pchar ')') symbol)
            )

        let atomicSymbol =
            (   (puint32
                  |>> NumManaSymbol)
            <|> (colorAtomCharFrom charset
                  |>> ColorManaSymbol)
            <|> (charReturn (charset.TwoLifeSymbol) TwoLifeSymbol)
            <|> (charReturn (charset.SnowSymbol)    SnowManaSymbol)
            <|> (letter
                  |>> (string >> VarManaSymbol))
            <|> groupSymbol
            )

        let halfSymbol =
            atomicSymbol .>> (pstring "/2") |>> HalfManaSymbol

        let hybrid1 lhs rhs =
            ManaSymbol.HybridManaSymbol (lhs, rhs)

        let hybridMany =
            pipe2
              (atomicSymbol .>> (pchar '/'))
              (sepBy1 atomicSymbol (pchar '/'))   // 白/青/黒 のような並列を認める
              (List.fold hybrid1)

        symbolRef :=
                attempt halfSymbol
            <|> attempt hybridMany
            <|> atomicSymbol

        groupSymbol

    let manaSymbol: Parser<_> =
        manaSymbolFrom symbolCharSet
        <|> manaSymbolFrom SymbolCharSet.Ja

    let manaCost: Parser<_> =
        many manaSymbol

    let colorIdent charset =
        betweenParen '[' ']'
          (sepBy (colorAtomCharFrom charset) (skipChar '/'))

    let typeLineTmpl (charset: SymbolCharSet) supertypeList cardtypeList subtypeList =
        let separator =
            skipMany1 (anyOf (charset.TypeLineSeparator)) >>. blankMany

        skipNewline >>. 
          tuple4
            (opt (colorIdent charset)
              .>> blankMany)
            (supertypeList
              .>> optional skipDotOrBlank)
            (cardtypeList
              .>> blankMany)
            (opt
              (separator
              >>. subtypeList
              //TODO: rarity
              .>> blankMany
              ) |>> Option.getOr [])

    // '*' を含む式
    let starExpr =
        regex @"[0-9XYZ*+ \t-]+"

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
            attempt (skip powTou)
            <|> attempt (skip loyalty)
            <|> attempt flavorTextKeyword
            <|> skipEmptyLine

        let line =
            notFollowedBy forbidden >>. restOfLine false

        commentLines
        >>. sepBy line skipNewlineAndCommentLines
        |>> Str.join "\r\n"

    module Ja =
      let supertypeList =
          let p =
              (Supertype.Cases, Supertype.JaNames)
              ||> List.map2 (fun st name -> skipString name >>% st)
              |> choice

          sepBy p (optional skipDotOrBlank)

      let cardtypeList =
          let p =
              (CardType.Cases, CardType.JaNames)
              ||> List.map2 (fun ct name -> skipString name >>% ct)
              |> choice

          sepBy1 p skipDotOrBlank

      /// 英名併記を許可
      let subtype =
          let enName =
              betweenParen '(' ')' (many letter)
          many1Chars letter .>> optional enName

      let subtypeList =
          sepBy subtype (optional skipDotOrBlank)

      let typeLine =
          typeLineTmpl
            SymbolCharSet.Ja supertypeList cardtypeList subtypeList

    /// カード
    let cardSpec: Parser<_> = parse {
        let! name     = cardName         .>> blankMany
        let! manaCost = manaCost         .>> blankMany
        let! typeLine = Ja.typeLine      .>> skipNewline
        let! ruleText = ruleTextLines    .>> skipNewline
        let! ptl      = powTouLoyalty    .>> skipNewline

        let (colorIdent, supertypes, cardtypes, subtypes) = typeLine
        let colorIdent = colorIdent |> Option.map colorFromAtoms
        return {
            Name       = string name
            ManaCost   = manaCost
            Color      = colorFromManacostOrIdentity (manaCost, colorIdent)
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
        return
          { Card = card
            Expansions = Map.empty // TODO: analyze comms and make this Map
          }
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

    let parseCardList text =
        let p =
          manaCost
        run p text

  open System.IO
  let main () =
      let text = File.ReadAllText (@"D:\Docs\archive\_nobak\__testdata.txt")
      let result =
          CardSyntaxParser.parseCardList text
      match result with
      | ParserResult.Success (value, stat, pos) ->
          ()
      | ParserResult.Failure (message, err, stat) ->
          ()
      ()
