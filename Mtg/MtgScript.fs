namespace Mtg

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Xml.Linq
open System.Net
open Basis.Core
open Util
open Util.Collections

module SpoilerProc =
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
          // "Name: ", "Card Name"
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
                  yield
                    ( "Card Name:\t" + name + "\n"
                    + (descriptionToSpoilerCardItem description))
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
          let rw4 = colorsAndCMCFromManacostInMWS "4RW"
          assert (rw4 = (["W"; "R"], 6))
          let hybrid5 = colorsAndCMCFromManacostInMWS "3R/GR/U"
          assert (hybrid5 = (["U"; "R"; "G"], 5))
          ()
      ()

  let renamePicsMain () =
      let dir = @"D:\Game\MWS\Pics\JOU"
      dir |> renamePics
      dir |> spoilerTextFromRss

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
