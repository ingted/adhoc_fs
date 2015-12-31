namespace MTG

open System
open System.Text
open Basis.Core
open Uedai.Utilities

open FParsec
open MTG.StrExp
open MTG.StrExp.Japanese

module Handmade =
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
    open Uedai.Utilities.FParsec

    /// カード名
    let cardName =
        let content =
                (attempt anyInLine |>> (fun jp -> { Jp = jp; En = "" }))
            <|> (pipe3 anyInLine (pchar '/') anyInLine
                    (fun jp slash en -> { Jp = jp; En = en }))

        between (pchar '《') (pchar '》') content

    let colorAtomJpChar =
        anyOf (ColorAtom.JaChars |> Str.join "")
        |>> (ColorAtom.FromJaChar >> Option.get)
    let colorAtomEnChar =
        anyOf (ColorAtom.Chars |> Str.join "")
        |>> (ColorAtom.FromChar >> Option.get)
    let colorAtomChar =
        (attempt colorAtomJpChar) <|> colorAtomEnChar

    /// マナ・シンボル (日本語)
    let manaSymbolJp : Parser<_> =
        let symbol, symbolRef =
            FParsec.Primitives.createParserForwardedToRef ()

        let groupSymbol =
            between (pchar '(') (pchar ')') symbol

        let atomicSymbol =
                (puint32
                    |>> NumManaSymbol)
            <|> (colorAtomJpChar |>> ColorManaSymbol)
            <|> (charReturn 'Φ' TwoLifeSymbol)
            <|> (charReturn '氷' SnowManaSymbol)
            <|> (CharParsers.letter
                    |>> (string >> VarManaSymbol))
            <|> groupSymbol

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

    let manaCostJp =
        many manaSymbolJp

    /// 色識別子
    let colorIdent =
        between (pchar '[') (pchar ']')
            (sepBy colorAtomChar (pchar '/'))

    /// TODO: 日本語の特殊タイプの分析は単純にはいかない
    let supertypesJp =
        sepBy (anyInLine |>> Supertype.FromJaName) (opt (pchar '・'))
        |>> List.choose id  // TODO: 無視されたものを警告

    let cardTypesJp =
        sepBy1 (anyInLine |>> CardType.FromJaName) (opt (pchar '・'))
        |>> List.choose id  // TODO: 無視されたものを警告

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
        pipe4
          ((opt colorIdent) .>> ws)
          supertypesJp
          (cardTypesJp .>> ws)
          (((many1 (anyOf "―-－～~")) >>. ws) >>. (subtypesJp .>> ws))
          (fun ci supertypes cardtypes subtypes ->
              (ci, supertypes, cardtypes, subtypes)
          )

    /// P/T、loyalty
    /// */*+1 とかは未定義とみなす
    let powTou : Parser<_> =
        pipe2
          (pint32 .>> pchar '/')
          pint32
          (fun p t -> (Some (p, t), None))
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

    let cardSpec : Parser<_> =
        let f name manacost typeline text (pt, loyalty) =
            let (colorIdent, supertypes, cardtypes, subtypes) = typeline
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

        pipe5
          (cardName                     .>> ws)
          (manaCostJp                   .>> (ws .>> newline))
          (typeline                     .>> newline) 
          (anyStringWithoutEmptyLine    .>> newline)
          ((attempt powTou <|> loyalty) .>> newline)
          f

    // regular, flip, double-faced, splits
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

    let cardSingle: Parser<_> =
        let f card ft comms =
            {
              Card = card
              Expansions = Map.empty // TODO: analyze comms and make this Map
            }

        pipe3
          cardBody (opt flavorText) (many commentline) f

    let cardList: Parser<_> =
        sepBy cardSingle skipNewline

    let parseCardList name text =
        CharStream.ParseString (text, 0, text |> Str.length, cardList, (), name)

  open System.IO
  let main () =
      let text = File.ReadAllText (@"D:\Docs\archive\_nobak\__testdata.txt")
      let reply =
          CardSyntaxParser.parseCardList "MTG Handmade cardlist (test)" text
      let result =
          match reply.Status with
          | ReplyStatus.Ok ->
              Result.Success (reply.Result)
          | ReplyStatus.Error
          | ReplyStatus.FatalError ->
              Result.Failure (reply.Error)
          | _ -> failwith "unknown status"
      match result with
      | Result.Success ls ->
          let cardList = ls
          printfn "%d cards found." (cardList |> List.length)
      | Result.Failure err ->
          printfn "Error: %A" (err)
      ()
