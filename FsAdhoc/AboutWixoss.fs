module AboutWixoss

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

open System.Net

open Basis.Core

let urlWixoss = @"http://www.takaratomy.co.jp/products/wixoss"
let urlCardDetail id = urlWixoss + @"/card/card_detail.php?id=" + (string id)
let urlCardImage exp codename = urlWixoss + @"/images/card/" + exp + "/" + codename + ".jpg"

let optionFromBoolPair (b, v) = if b then Some v else None

// Cockatrice でのみ動かし、MWS は使わないのであれば、カード名に日本語を利用しても問題ない
// カード名のアルファベットを半角にしたい (?)

module Something =
    let ptnColorName = @"[無白赤青緑黒]"

    // アイコンの名前は画像ファイル名
    let symbolFromIconName = function
        | "icon_txt_null" -> "{1}"
        | "icon_txt_white" -> "{W}"
        | "icon_txt_red" -> "{R}"
        | "icon_txt_blue" -> "{U}"
        | "icon_txt_green" -> "{G}"
        | "icon_txt_black" -> "{B}"
        | "icon_txt_down" -> "{T}"
        | "icon_txt_regular" -> "[常]"
        | "icon_txt_starting" -> "[起]"
        | "icon_txt_arrival" -> "[出]"
        | "icon_txt_burst" -> "[☆]"
        | _ -> failwith "invalid icon name"
    let textFixing text =
        let text' = Regex.Replace (text, @"(?:\r?\n?)<br ?/?>(?:\r?\n?)|　(?=\<img)", "\r\n")
        //let text' = Regex.Replace (text', @"　\[", "\r\n[")   // memo: 能力の切れ目が全角空白になっていることがある
        let text' =
            Regex.Replace (text',
                """<img src=".*?/([^/]+)\.png".*?>""",
                (fun (m : Match) -> symbolFromIconName m.Groups.[1].Value))
        text'

    let numberFromFullspaceDigit = function
        | "０" -> 0 | "１" -> 1 | "２" -> 2 | "３" -> 3 | "４" -> 4 | "５" -> 5 | "６" -> 6 | "７" -> 7 | "８" -> 8 | "９" -> 9
        | _ -> failwith "invalid digit"
    let tryParseInt s =
        Int32.TryParse (Regex.Replace(s, "[０１２３４５６７８９]", (fun (m : Match) -> m.Value |> numberFromFullspaceDigit |> string)))
        |> optionFromBoolPair

    let colorsFromColorKanji = function
        | "無" -> []
        | "白" -> ["W"]
        | "赤" -> ["R"]
        | "青" -> ["U"]
        | "緑" -> ["G"]
        | "黒" -> ["B"]
        | _ -> failwith "invalid card color"

    let tablerowFromKind = function
        | "シグニ" -> 0    // forward line
        | "ルリグ" -> 1    // backward line
        | "アーツ" -> 1
        | "スペル" -> 3    // stack
        | _ -> failwith "invalid card kind"

    type CardInfo = {
        Exp : string
        Number : int
        Id : int
        Name : string
        Kind : string
        Type : string
        Color : string
        Level : int option
    } with
        member this.CodeName = sprintf "%s-%03d" this.Exp this.Number

    (*
    type CardText = {
        Skill : string
        EnaBurst : string
        Flavor : string
    }
    type LrigCard = {
        Info : CardInfo
        Level : int
        Limit : int
        GlowCost : string
        Text : string
    }
    type ArtsCard = {
        Info : CardInfo
        Cost : string
        Type : string option
        LimitCond : string option
        Text : string
    }
    type SigniCard = {
        Info : CardInfo
        Type : string option
        LimitCond : string option
        Text : string
    }
    type SpellCard = {
        Info : CardInfo
        Cost : string
        Text : string
    }
    type Card =
        | LrigCard of LrigCard
        | ArtsCard of ArtsCard
        | SigniCard of SigniCard
        | SpellCard of SpellCard 
    //*)

    let spaces = @"[ 　\t\r\n]*"
    let charOrCrlf = @"(?:.|\r|\n|<br ?/?>)"
    let textCapture = "(" + charOrCrlf + "+?)"
    let patternOfCardlist =
        """<td>(W[DX]\d+)\-0*(\d+)</td>"""
        + spaces + """<td .*?><a .+?href="\./card_detail\.php\?id=(\d+)">(.+?)</a></td>"""
        + spaces + """<td .*?>(""" + ptnColorName + """)</td>"""
        + spaces + """<td>(ルリグ|シグニ|アーツ|スペル)</td>"""
        + spaces + """<td>(.*?)</td>"""
        + spaces + """<td>(\d*|-)</td>"""
    let patternOfDetail =
        """<td .*?class="card_skill card_text">""" + spaces + textCapture + spaces + "</td>" + spaces + "</tr>"
        + "(?:" + spaces + "<tr>" + spaces + "<td .*?>" + spaces + textCapture + spaces + "</td>" + spaces + "</tr>)?"
        + "(?:" + spaces + "<tr>" + spaces + "<td .*?>" + spaces + textCapture + spaces + "</td>)?"
    let regOfDetail = new Regex (patternOfDetail)

    /// カードリストの html テキストから、カードセット情報を得る
    let CardsetFromHtml (html : string) =
        let matches = Regex.Matches(html, patternOfCardlist)

        let list = new List<CardInfo>()
        for i in 0..(matches.Count - 1) do
            let m = matches.[i]
            let expansion = m.Groups.[1].Value
            let cardNumber = m.Groups.[2].Value |> tryParseInt |> Option.get
            let cardId = m.Groups.[3].Value |> tryParseInt |> Option.get
            let cardName = m.Groups.[4].Value
            let cardColor = m.Groups.[5].Value
            let cardKind = m.Groups.[6].Value
            let cardType = m.Groups.[7].Value
            let cardLevel = m.Groups.[8].Value |> tryParseInt

            let info = {
                CardInfo.Exp = expansion
                Number = cardNumber
                Id = cardId
                Name = cardName |> Str.trim
                Kind = cardKind
                Type = if cardType <> "-" then cardType else ""
                Color = cardColor
                Level = cardLevel
            }
            assert (i = 0 || list.[0].Exp = info.Exp)
            list.Add info
            
        list.Sort (fun lhs rhs -> lhs.Number - rhs.Number)
        let exp = (if list.Count > 0 then Some list.[0].Exp else None)
        (exp, List.ofSeq list)

    type CardDetail = {
        Info : CardInfo

        // グロウコスト or コスト
        Cost : string option
        Limit : int option
        Power : int option
        LimitCond : string option
        // 有 or 無
        //Guard : bool
        SkillText : string
        EnaText : string
        FlavorText : string
    }
    
    let ManacostFromCostText cost =
        let matches = Regex.Matches (cost, "(" + ptnColorName + ")×(\d+)")
        if matches.Count = 0 then
            None
        else
            let mutable buf = ""
            for i in 0..(matches.Count - 1) do
                let m = matches.[i]
                let n = m.Groups.[2].Value |> tryParseInt |> Option.get
                let colors = m.Groups.[1].Value |> colorsFromColorKanji
                match colors with
                | [] -> buf <- (string n) + buf
                | [color] ->
                    let sym = color // <manacost> の中では { } で括らなくていい
                    buf <- (String.replicate n sym) + buf
                | _ -> failwith "multicolored symbol is unexpected"
            Some buf
            
    /// テーブル要素から情報を抽出する
    let getFromTable html s =
        let m = Regex.Match (html, "<th>" + s + @"</th>[ \t\r\n]*<td>(.+?)</td>")
        if m.Success
            then
                let v = m.Groups.[1].Value
                if v <> "-" then Some v else None
            else None
    /// (未使用) タイトルの右上にある希少度表示を抽出する
    let getRarityFromDetail html =
        let ptn = """<div class="card_rarity txt_yellow">""" + spaces + "(.+?)" + spaces + "</div>"
        let m = Regex.Match(html, ptn)
        m.Groups.[1].Value
    /// 詳細カード情報から詳しいデータを取り出す
    let DetailFromHtml (info : CardInfo) (html : string) =
        let m = regOfDetail.Match html
        let (skill, enaBurst, flavor) = // 問題：この通りにはマッチしない (エナバーストがないときに [1], [2] に入ってしまうバグがある、結果は同じなので放置している)
            match m.Groups.Count - 1 with
            | 0 -> ("", "", "")
            | 1 -> (m.Groups.[1].Value, "", "")
            | 2 -> (m.Groups.[1].Value, "", m.Groups.[2].Value)
            | 3 -> (m.Groups.[1].Value, m.Groups.[2].Value, m.Groups.[3].Value)
            | _ -> failwith "invalid html format"
        let costSrc =
            if info.Kind = "ルリグ" then "グロウコスト" else "コスト"
        {
            CardDetail.Info = info
            Cost  = getFromTable html costSrc
            Limit = getFromTable html "リミット" |> Option.bind tryParseInt
            Power = getFromTable html "パワー" |> Option.bind tryParseInt
            LimitCond = getFromTable html "限定条件"
            SkillText = skill |> textFixing
            EnaText = enaBurst |> textFixing
            FlavorText = flavor
        }

    // 実際やってみる
    // カードリストの html は手動でダウンロードしておく
    let sample() =
        let dir = "D:/Docs/downloads/WIXOSS"
        let exp = "WX01"

        let html = File.ReadAllText(dir + "/" + exp + ".html")
        let (_, set) = CardsetFromHtml html
        printfn "Set: %s (%d cards)" exp (set.Length)

        use webcl = new WebClient()
        let buf = new Text.StringBuilder()

        for info in set do
            let codename = info.CodeName
            let tablerow = tablerowFromKind info.Kind
            printfn "proceess %s ([名]：``%s``)" codename info.Name

            // 詳細
            let htmlDetail =
                let pathCache = dir + "/CardDetails/" + (string info.Id) + ".htm"
                if File.Exists pathCache |> not then
                    webcl.DownloadFile (urlCardDetail info.Id, pathCache)
                    System.Threading.Thread.Sleep 3000
                File.ReadAllText pathCache
                
            // 画像
            let pathImageCache = dir + "/" + exp + "/" + codename + ".full.jpg"
            if File.Exists pathImageCache |> not then
                webcl.DownloadFile (new Uri(urlCardImage exp codename), pathImageCache)
                System.Threading.Thread.Sleep 3000

            // 詳細からデータをもらう
            // いろいろオーバーロードして使用する
            let detail = DetailFromHtml info htmlDetail

            let colorTags =
                info.Color
                |> colorsFromColorKanji
                |> List.map (fun ch -> "<color>" + ch + "</color>")
                |> Str.join ""
            let manacost =
                detail.Cost |> Option.bind ManacostFromCostText  |> Option.getOr ""
            let typeline =
                // サブタイプっぽいもの：カードタイプ、限定条件
                let subtypy =
                    [info.Type; detail.LimitCond |> Option.getOr ""]
                    |> List.filter (Str.isNullOrEmpty >> not)
                    |> Str.join "・"
                (info.Kind + (if subtypy.Length > 0 then (" - " + subtypy) else ""))
            let ptTag =
                let ptElem =
                    match info.Kind with
                    | "ルリグ" ->
                        Some ((detail.Limit |> Option.getOr 0), (info.Level |> Option.getOr 0))
                    | "シグニ" ->
                        Some ((detail.Power |> Option.getOr 0), (info.Level |> Option.getOr 0))
                    | _ -> None
                match ptElem with
                | Some (p, t) -> "<pt>" + (string p) + "/" + (string t) + "</pt>"
                | None -> ""
            let text =
                [detail.SkillText; detail.EnaText; detail.FlavorText; "[名]：``" + info.Name + "``"]
                |> List.filter (Str.isNullOrEmpty >> not)
                |> Str.join ("\r\n----\r\n")

            buf.AppendLine (
                (sprintf
                    """<card> <name>%s</name><set>%s</set>%s<manacost>%s</manacost><type>%s</type>%s<tablerow>%d</tablerow><text>%s</text> </card>"""
                    codename exp colorTags manacost typeline ptTag tablerow text)
            ) |> ignore

            ()

        File.WriteAllText (dir + "/" + exp + ".spoiler.txt", buf.ToString())

    (*
    type CardData = {
        Id : int
        Exp : string
        Number : int
        FullName : string

        // "シグニ", "ルリグ", "アーツ", "スペル"
        Kind : string
        Type : string
        Color : string
        Level : int
        GlowCost : int
        Cost : int
        Limit : int
        Power : int
        LimitCondition : string
        Guard : string
        SkillText: string
        EnaChargeText : string
        FlavorText : string
    }
    //*)

    /// カードセット情報から、各カード詳細と画像をダウンロードしてスポイラー文章を作成する
    let MakeSpoilerFromCardset ((exp : string), (infos :  CardInfo list)) =
        use webcl = new WebClient()

        (*
        let imageFileNameReg =
            new Regex """<img src="/products/wixoss/images/card/.+?/(.+?\.jpg)>"""
        let getFromTable html s =
            let m = Regex.Match (html, "<th>" + s + "</th>[ \t\n]*<td>(.+?)</td>")
            if m.Success then
                if m.Value = "-" then
                    None
                else
                    Some m.Value
            else
                None//*)

        for card in infos do
            //let html = webcl.DownloadString (urlCardDetail card.Id)
            
            //let imageFileName = imageFileNameReg.Match(html).Groups.[1].Value

            ()
        ()

