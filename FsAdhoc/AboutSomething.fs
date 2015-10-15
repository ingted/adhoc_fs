module AboutSomething

open System
open System.Net
open System.Xml
open System.Xml.Linq
open System.Text
open System.Text.RegularExpressions
open Basis.Core
open Basis.Core.Xml.NamespaceLess
open Basis.Core.Xml.NamespaceLess.XPath
open Uedai.Utilities

module DownloadCardDataFromSkyGalleonWiki =
    let statusTypes = ["HP"; "AT"; "AG"]
    let skillTypes = ["特技"; "前列"; "中列"; "後列"]
    let rarities = ["C";"UC";"R";"R+";"SR";"SR+";"LE"; "LE+"]

    ///ステータス補正値表 (Lv1,10,20,30,40)
    let statusCoeffs rarity statusName =
        assert (statusTypes |> Seq.contains statusName)
        assert (rarities |> Seq.contains rarity)
        match statusName, rarity with
        | "AG", _ ->  [1.0; 1.00; 1.0; 1.00; 1.00]
        | _, "C" ->   [0.8; 0.95; 1.1; 1.15; 1.20]
        | _, "LE"
        | _, "LE+" -> [0.8; 0.89; 1.0; 1.12; 1.25]
        | _, _ ->     [0.8; 0.89; 1.0; 1.10; 1.20]
    ///基本のクリティカル率と係数の組
    let criticalPotentials = function
        | "SR" | "SR+" -> ((60, 1.0), (30, 1.2), (10, 1.5))
        | "R" | "R+"   -> ((65, 1.0), (25, 1.2), (10, 1.5))
        | "C" | "UC"   -> ((75, 1.0), (20, 1.2), ( 5, 1.5))
        | "LE"         -> ((75, 1.0), (20, 1.5), ( 5, 2.0))
        | _ -> failwith "unknown rarity"
    let criticalCoeffs rarity =
        let (_, c0), (_, c1), (_, c2) = criticalPotentials rarity
        (c0, c1, c2)
    ///クリティカル係数と思われる係数トリプルを可能な限り見つける
    let findAllCriticalCoeffTriples rarity =
        let eq d1 d2 = (abs(d1 - d2)) < 0.001
        ///1.5倍するときは元の0.5から0.025が表れるが、それは0.5に切り上げる
        let myRound d =
            if eq (d % 0.1) 0.025 then d + 0.025 else d
        let (_, c1, c2) = criticalCoeffs rarity
        fun ls ->
            ls |> List.choose (fun baseCoeff ->
                let crLv1Coeff = (c1 * baseCoeff) |> myRound
                let crLv2Coeff = (c2 * baseCoeff) |> myRound
                OptionDefaultOps.option { 
                    let! coeff1 = ls |> List.tryFind (eq crLv1Coeff)
                    let! coeff2 = ls |> List.tryFind (eq crLv2Coeff)
                    return (baseCoeff, coeff1, coeff2)
                }
            )

    type RangeSide = | NearSide | OppoSide | BothSide
    with
        static member private Names = [NearSide,"自陣"; OppoSide,"敵陣"; BothSide,"両陣"] |> Map.ofList
        override self.ToString() = RangeSide.Names.TryFind self |> Option.get
        static member TryParse s = RangeSide.Names |> Map.inverse s |> Seq.tryFirst

    type Bit = | X | O
    with
        static member ofBool = function | true -> X | false -> O
        static member toInt = function | X -> 1 | O -> 0
        //override self.ToString() = match self with | X -> "1" | O -> "0"
        
    ///文章順(左上→右下)から先攻配置順(右上から左下)に並び替え
    ///敵味方に作用する効果の範囲は後攻側として書かれている
    let sortBitsInPlacementOrderOfNearSideFromTextOrder rangeSide (bits': Bit list) =
        assert (bits' |> List.length = 9)
        let order =
            match rangeSide with
            | NearSide  -> [2;5;8;  1;4;7;  0;3;6]
            | _         -> [0;3;6;  1;4;7;  2;5;8]
        let a = order |> List.map (fun i -> bits'.[i])
        (a.[0], a.[1], a.[2]), (a.[3], a.[4], a.[5]), (a.[6], a.[7], a.[8])

    ///範囲の表現
    ///自陣への相対範囲でも○/●は使わない
    let formatRangeInfo (bits, relationality, side) =
        let encode() =
            let codeRel  = function | true -> "Re" | false -> "Ab"
            let codeSide = function | NearSide -> "Ns" | OppoSide -> "Os" | BothSide -> "Bs"
            let codeBits (r0, r1, r2) =
                let f (b0, b1, b2) = [b0; b1; b2] |> List.map (Bit.toInt >> string) |> Str.join ""
                let s = [r0; r1; r2] |> List.map f |> Str.join ""
                s
            (codeRel relationality) + (codeSide side) + (codeBits bits)
        (*
        let prefix = (string side) + (if relationality then "相対" else "絶対")
        match bits, relationality, side with
        | ((X,O,X), (X,O,X), (X,O,X)), false, _ -> (string side) + "上下段6"  //クロノス前列など
      //| ((X,O,O), (X,O,O), (X,O,O)), false, _ -> (string side) + "上段3"
        | ((O,X,O), (O,X,O), (O,X,O)), false, _ -> (string side) + "中段3"
      //| ((O,O,X), (O,O,X), (O,O,X)), false, _ -> (string side) + "下段3"
        | ((O,X,O), (O,O,O), (O,O,O)), true, _  -> (string side) + "前面1"
        | ((O,X,O), (O,X,O), (O,O,O)), true, NearSide -> "前面2"
        | ((O,O,O), (O,O,O), (O,X,O)), true, NearSide -> "後面1"
        | ((O,O,O), (O,X,O), (O,X,O)), true, NearSide -> "後面2"
        | ((O,X,O), (O,O,O), (O,X,O)), true, NearSide -> "前後2"
        | ((O,X,O), (O,X,O), (O,X,O)), true, NearSide -> "前後3"
        | ((O,O,O), (X,O,X), (O,O,O)), true, NearSide -> "上下2"
        | ((O,O,O), (X,X,X), (O,O,O)), true, NearSide -> "上下3"
        | ((O,X,O), (O,X,O), (O,O,O)), true, OppoSide -> "突進2"  //フレイ+前列など
        | ((O,X,O), (O,X,O), (O,X,O)), true, OppoSide -> "突進3"
        | ((X,X,X), (O,O,O), (O,O,O)), _, _ -> prefix + "前列3"
        | ((O,O,O), (X,X,X), (O,O,O)), _, _ -> prefix + "中列3"
        | ((O,O,O), (O,O,O), (X,X,X)), _, _ -> prefix + "後列3"
        | ((X,X,X), (X,X,X), (O,O,O)), _, _ -> prefix + "前中列6"
        | ((X,X,X), (O,O,O), (X,X,X)), _, _ -> prefix + "前後列6"
        | ((O,O,O), (X,X,X), (X,X,X)), _, _ -> prefix + "中後列6"
        | ((X,X,X), (X,X,X), (X,X,X)), _, _ -> prefix + "全列9"
        | ((O,O,O), (O,X,O), (O,O,O)), _, _ -> prefix + "中央1"
        | ((X,X,X), (O,X,O), (O,O,O)), _, _ -> prefix + "⊥字4"
        | ((X,X,X), (X,X,X), (O,X,O)), _, _ -> prefix + "凸字7"
        | ((X,X,X), (X,O,X), (O,O,O)), _, _ -> prefix + "凹字5"   //クルワッハ後列
        | ((O,X,O), (X,O,X), (O,X,O)), _, _ -> prefix + "十字4"
        | ((O,X,O), (X,X,X), (O,X,O)), _, _ -> prefix + "十字5"
      //| ((X,O,X), (O,O,O), (X,O,X)), _, _ -> prefix + "四隅4"
        | ((X,O,X), (O,X,O), (X,O,X)), _, _ -> prefix + "Ｘ字5"   //ファフニール中列
      //| ((X,X,X), (O,X,O), (X,O,X)), _, _ -> prefix + "銀形6"   //ヘスティア後列、銀の進む方向
        | (r0, r1, r2), _, _ ->
            let f (b0, b1, b2) = [b0; b1; b2] |> List.map string |> Str.join ""
            let s = [r0; r1; r2] |> List.map f |> Str.join "/"
            prefix + "[" + s + "]"
        //*)
        encode()
        
    /// (Lv??～) という文字列にマッチするはずの表現
    let regLvClause = @"(?:[ 　\t]*(?:\(\w+[0-9\?]+[～~]*\))?[ 　\t]*)"

    ///wikiの文章内の属性値表記を抽出する
    let tryFindEmbeddedAttribute key pattern groupid text =
        let key = key |> Regex.Escape
        //「キー：値」形式
        let pattern1 = key + @"[:：](?:.|\r|\n)*?<!--@+-->(" + pattern + @")(?=</span>)"
        //新テンプレにおけるテーブル
        //テーブル内のキーの要素の直後の要素、を抽出する
        let pattern2 = "(?<=<!--@+-->)" + key + "</span>(?:.|\r|\n)*?<!--@+-->(" + pattern + ")(?=</span>)"

        let m1 = Regex.Match(text, pattern1)
        m1.Groups.[groupid + 1].TryValue
        |> Option.union' (fun() ->
            let m2 = Regex.Match(text, pattern2)
            m2.Groups.[groupid + 1].TryValue
        )
    //let getOrError key = Option.getOrElse <| fun() -> failwith ("error: " + key + " not found")

    //rarityの取得
    let tryFindRarity source =
        let pattern = @"LE|SR\+|SR|R\+|R|UC|C"
        source
        |> tryFindEmbeddedAttribute "レアリティ" pattern 0
    //コストの取得
    let tryFindCost source =
        source |> tryFindEmbeddedAttribute "コスト" "[2-9]" 0

    //枚数制限
    let tryFindNumberRestriction rarity source =
        assert (rarities |> Seq.contains rarity)
        let pattern = """(1|2|１|２|一|二|[無な]?し?)枚?制?限?"""
        match rarity with
        | "LE" | "LE+" -> Some "1"
        | "SR" | "SR+" ->
            source
            |> tryFindEmbeddedAttribute "制限" pattern 1
            |> Option.bind (function
                | "2" | "２" | "二" -> Some "2"
                | "なし" | "無し" | "無" -> Some ";"
                | _ -> None)
        | _ -> Some ";"

    //配置制限
    let tryFindLocationRestriction source =
        source
        |> tryFindEmbeddedAttribute "配置" ".*?" 0
        |> Option.bind (fun s ->
            if s = "すべて" || s = "全て" || s = "全列" then
                Some ";"
            else
                let ls = [ for row in ["前列"; "中列"; "後列"] do
                            if s |> Str.contains row |> not then yield row ]
                if [1; 2] |> Seq.contains (ls |> List.length) |> not then // 禁止列は1列か2列のはず
                    printfn "warning: extracting location restriction was something wrong"
                    None
                else
                    Some (ls |> Str.join ";")
        )
    //分類
    let tryFindGender source =
        source
        |> tryFindEmbeddedAttribute "分類" "男性|女性|そ?の?他|不明" 0
        |> Option.bind (function
            | "男性" | "女性" | "その他" as s -> Some s
            | "他" | "不明" -> Some "その他"
            | _ -> None
        )
    //イラストレータ
    //形式が特殊なので抽出しづらい。新旧とも、「イラストレータ/(名前)」の形式。
    let tryFindIllustrator source =
        source |> Str.tryIndexOf "イラストレーター/"
        |> Option.bind (fun index ->
            let _, after = source |> Str.splitAt index
            let m = Regex.Match(after, """(?<=<!--@+-->)(.*?)(?=</span>)""")
            m.TryValue
        )

    //HP,AT,AGの取得
    let tryFindStatusValue statusType rarity source =
        let sourceTableRow =
            Regex.Match(source, statusType + "</td>" + @"(?:.|\r|\n)*?</tr>").Value
        let values =
            [ for m in Regex.Matches(sourceTableRow, @"<!--@+-->(\d*)") ->
                m.Groups.[1].Value |> Double.TryParse |> Option.trialResult ]
        if (values |> List.length < 5) then
            do printfn "warning: status %s counts %d, maybe too less" statusType (values|> List.length)

        let coeffs = statusCoeffs rarity statusType
        values
        |> Seq.truncate 5 //Lv1,10,20,30,40
        |> Seq.tryFindIndex (Option.isSome)
        |> Option.map (fun i ->
            let value = values.[i] |> Option.get
            ((value / coeffs.[i])) |> round |> int
        )

    let tryFindStatusValues rarity html =
        [ for statusType in statusTypes -> tryFindStatusValue statusType rarity html ]

    ///ソースの攻撃範囲の部分だけ切り出す
    ///i1: 効果1なら1、効果2なら2
    let tryFindSourceRange i1 source =
        assert (i1 = 1 || i1 = 2)
        Regex.Match(source, "効果" + (i1 |> string) + "範囲：" + @"(?:.|\r|\n)*?<table>(?:.|\r|\n)*?</table>").TryValue
    let tryFindSkillName skillType =
        assert (skillTypes |> Seq.contains skillType)
        tryFindEmbeddedAttribute skillType @" *[^(（ ]+? *" 0 >> Option.map Str.trim
    let tryFindProbability sourceRange = OptionDefaultOps.option {
            let! value = sourceRange |> tryFindEmbeddedAttribute "発動確率" @"(\d+)[%％]" 1
            let! value = value |> Int32.TryParse |> Option.trialResult
            return value
        }
    let tryFindEffectColor i1 = tryFindEmbeddedAttribute ("効果" + (string i1) + "属性") @"無|緑|赤|青|黒" 0
    let tryFindRangeType i1 = tryFindEmbeddedAttribute ("効果" + (i1 |> string) + "範囲") @"絶対|相対" 0
    let findRangeSide effectText sourceRange =
        //note that both-side effects are now written blue as similar to opposite-side effects in the wiki
        if sourceRange |> Str.tryIndexOf ("color:blue") |> Option.isNone then
            NearSide
        else
            match effectText |> Str.tryIndexOf "敵味方" with
            | Some _ -> BothSide
            | None -> OppoSide

    let tryFindEffectText sourceSkill =
        //フックがないので抽出しづらい。レベルに応じて複数個書かれているはず。
        //効果1属性のキーより前にあるなかで、最後から2番目のものが正解のはず (最後は発動確率)
        //発動確率のキーを検索すると、説明文中のものがヒットしてしまいうる。キーに正規表現を認めて [:：] をつければいいけど。
        //要素の最初が全角空白で始まっているものが多いが、編集者の手癖に依存したくない。
        sourceSkill
        |> Str.tryIndexOf "効果1属性"
        |> Option.bind (fun index ->
            let beforeProb, _ = sourceSkill |> Str.splitAt index
            let elems =
                [ for m in Regex.Matches(beforeProb, @"<!--@+-->" + regLvClause + @"?(?:[　 \t]*)(.*?)</span>")
                    -> m.Groups.[1].Value ]
            elems |> Seq.tryNth ((elems |> Seq.length) - 2)
            )
            
    let tryFindRangeInfo rangeType rangeSide sourceRange = OptionDefaultOps.option {
        let matches = Regex.Matches(sourceRange, @"■|●|□|○")
        if matches.Count >= 9 then
            let bits =
                [ for i in 0..8 -> matches.[i].Value = "■" || matches.[i].Value = "●" ]
                |> List.map Bit.ofBool
                |> sortBitsInPlacementOrderOfNearSideFromTextOrder rangeSide
            let relationality = (rangeType = "相対")
            return formatRangeInfo (bits, relationality, rangeSide)
        }
        
    ///クリティカルを持つか否か
    ///特技なら探さなくていいがfalseが返るはず
    ///memo:クリティカル発動率上昇によりクリティカルが起こるかどうかは表記から判断不能。
    let findCriticalPossibility sourceRange =
        //表の中のクリティカル率と思われる数値のなかに100%でないものがあればクリティカルあり。
        //クリティカル率は必ず5の倍数
        [ for m in Regex.Matches(sourceRange, @"[1-9]*[05](?=[%％])") -> m.Value ]
        |> List.exists ((<>) "100")
    let findEffectCoeff criticalPossibility rarity sourceRange =
        ///クリティカル時の係数を排除する
        //クリティカル時の係数はある係数の定数倍なので、そういう関係の3つ組をすべて調べる
        //そのような3つ組の最初の値で最高のものを選択する
        let removeCoeffsOnCritical coeffs =
            if criticalPossibility then
                coeffs
                |> findAllCriticalCoeffTriples rarity
                |> List.map Tuple.fst'3
            else coeffs

        //表からすべての係数らしきものを抜き出して最大のものを探す
        [ for m in Regex.Matches(sourceRange, @"\d+\.\d+") ->
            m.Value |> Double.TryParse |> Option.trialResult ]
        |> List.choose id
        |> removeCoeffsOnCritical
        |> Seq.tryMax

    type EffectInfo = { Color: string; RangeInfo: string; CriticalPossibility: bool; Coeff: float option }
    with
        static member Undefined = { EffectInfo.Color = ";"; RangeInfo = ";"; CriticalPossibility = false; Coeff = None }
    type SkillInfo = { Type: string; Name: string option; Gauge: int option; Prob: int option; Text: string option; Effect1: EffectInfo option; Effect2: EffectInfo option }
    with
        static member Default = { SkillInfo.Type = ""; Name = None; Gauge = None; Prob = None; Text = None; Effect1 = None; Effect2 = None }

    ///効果の情報を収集する
    let tryFindEffectInfo rarity effectText sourceSkill =
        [ for i1 in [1; 2] -> OptionDefaultOps.option {
            let! effectText  = effectText
            let! color       = sourceSkill |> tryFindEffectColor i1
            let! sourceRange = sourceSkill |> tryFindSourceRange i1
            let! rangeType = sourceRange |> tryFindRangeType i1
            let  rangeSide = sourceRange |> findRangeSide effectText
            let! rangeInfo = sourceRange |> tryFindRangeInfo rangeType rangeSide
            let criticalPossibility = sourceRange |> findCriticalPossibility
            return{ EffectInfo.Color = color
                    RangeInfo = rangeInfo
                    CriticalPossibility = criticalPossibility
                    Coeff = sourceRange |> findEffectCoeff criticalPossibility rarity }
        } ]

    ///スキルの取得
    ///各要素は見つからなければ不明/Noneで進める気持ち
    let tryFindSkill skillType rarity source =
        assert (skillTypes |> Seq.contains skillType)
        Regex.Match(source, skillType + regLvClause + @"[:：](?:.|\r|\n)*?<hr").TryValue
        |> Option.map (fun sourceSkill ->
            let name = sourceSkill |> tryFindSkillName skillType
            if [Some "特技名"; Some "行動名"] |> Seq.contains name then //not written
                SkillInfo.Default
            else
                let gauge = option {
                    let! value = sourceSkill |> tryFindEmbeddedAttribute "ゲージ" @"\d+" 0
                    let! value = value |> Int32.TryParse |> Option.trialResult
                    return value
                    }
                let prob = sourceSkill |> tryFindProbability
                let effectText = sourceSkill |> tryFindEffectText
                let effects = sourceSkill |> tryFindEffectInfo rarity effectText

                assert (effects |> List.length = 2)
                let effect1, effect2 =
                    (List.nth effects 0), (List.nth effects 1)
                { SkillInfo.Type = skillType; Name = name;
                    Gauge = gauge; Prob = prob; Text = effectText;
                    Effect1 = effect1; Effect2 = effect2 }
        )

    let cardNameFromCsv csv =
        csv |> Str.splitBy "," |> Seq.nth 5

    let tryDownloadSource cardName =
        let url = @"http://www50.atwiki.jp/skygaleon_s/?page=" + (cardName |> WebUtility.UrlEncode)
        use webcl = new WebClient(Encoding = Encoding.UTF8)
        let tryDownloading (timeout: int) =
            try
                System.Threading.Thread.Sleep timeout
                Some <| webcl.DownloadString url
            with
                | _ as e ->
                    do printfn "error %s..." (e.Message)
                    None

        let source =
            option {
                for i in 1..3 do // try 3 times with some sleeps
                    let! s = tryDownloading (300 * (pown i 4))
                    return s
            }

        option {
            let! source = source
            if source |> Str.contains "は見つかりません" |> not then
                //cut off header text
                let meta, body = source |> Str.split2 "<body"
                return body }

    ///module for 'a option list
    module OptionList =
        ///set length just n, removing tail elements or appending None elements on the tail
        let extend n (self: _ option list) =
            let len = self |> List.length
            if n < len then
                self |> Seq.truncate n |> List.ofSeq
            else
                List.append self (List.ofSeq <| Seq.init (n - len) (fun _ -> None))
        ///fill None elements of the list with the correspond element of another one
        let merge self another =
            let len = max (self |> List.length) (another |> List.length)
            List.map2 Option.union (self |> extend len) (another |> extend len)

    ///蒼ガレwikiからカードデータ情報を抽出する
    let doIt csv =
        let cardName = cardNameFromCsv csv
        let foundData = option {
            printfn "begin %s" cardName
            let! source = tryDownloadSource cardName
            let! rarity = source |> tryFindRarity
            let cost = source |> tryFindCost
            let numberRestriction   = source |> tryFindNumberRestriction rarity
            let restrictedLocations = source |> tryFindLocationRestriction
            let gender = source |> tryFindGender
            let illustrator = source |> tryFindIllustrator

            let status = source |> tryFindStatusValues rarity
            let skills =
                [ for skillType in skillTypes ->
                    source |> tryFindSkill skillType rarity ]
            assert (skills |> List.length = 4)

            let hp, at, ag =
                status
                |> List.map (Option.map string)
                |> List.takeTuple3
            let gauge = option {
                let! skill = List.nth skills 0
                assert (skill.Type = "特技")
                let! gauge = skill.Gauge
                return gauge |> string
                }

            ///{効果} = {効果種別,色,範囲,抽出,変数,係数,クリティカルの有無}
            let listFromEffect = function
                | None -> [None; None; None; None; None; None; None]
                | Some (eff: EffectInfo) ->
                    [   None
                        Some eff.Color
                        Some eff.RangeInfo
                        None
                        None
                        eff.Coeff |> Option.map string
                        Some (if eff.CriticalPossibility then "1" else "0")
                    ]
            let listFromEffect =
                listFromEffect >> assert' (fun ls -> ls |> List.length = 7)
            ///{行動} = 発動率,文章,{効果1},{効果2}
            let listFromSkill (skill: SkillInfo) =
                if skill.Gauge.IsNone
                    && ["なし"; "無し"; "ない"; "無"; "存在しない"; "未定義"] |> Seq.contains (skill.Name |> Option.getOr "")
                then
                    [Some ";"; Some ";"]
                    @ [ for eff in [EffectInfo.Undefined; EffectInfo.Undefined] do yield! (Some eff) |> listFromEffect ]
                else 
                    [ skill.Prob |> Option.map string; skill.Text ]
                    @ [ for eff in [skill.Effect1; skill.Effect2] do yield! eff |> listFromEffect ]

            return
                //ID,種別番号,set,稀,色,カード名,cost,hp,at,ag,能力,特技ゲージ,
                [None; None; None; Some rarity; None; None; cost; hp; at; ag; None; gauge]
                //{特技},{前列},{中列},{後列},
                @ [ for skill in skills do
                        yield! skill |> Option.getOr SkillInfo.Default |> listFromSkill ]
                //制限枚数,配置禁止,分類,進化元,進化先,探索・討伐,絵師,声優,
                @ [numberRestriction; restrictedLocations; gender; None; None; illustrator; None]
            }

        //csvからリストへ
        let givenData = csv |> Str.splitBy "," |> List.ofArray |> List.map (Option.ifSat ((<>) ""))

        givenData
        |> OptionList.merge (foundData |> Option.getOr [])
        |> List.map (Option.getOr "")
        |> Str.join ","

    let mapFileName f path =
        let name = IO.Path.GetFileNameWithoutExtension(path)
        IO.Path.Combine(IO.Path.GetDirectoryName(path), (f name)) + IO.Path.GetExtension(path)

    ///csvファイルを更新する
    let updateCsvItems csvfile =
        let csvs = IO.File.ReadAllLines csvfile
        let updated = [ for csv in csvs -> doIt csv ]
        let filenameToSave = csvfile |> mapFileName (fun name -> name + "_updated")
        IO.File.WriteAllLines(filenameToSave, updated)

    (* todo:
    アルテミスなどのキャラカード、アフラマズダなどの変種テンプレでは、ステータスが取得できない
    抽出に失敗した場合のエラー表示
    //*)
    ()

module ProcLogicExpr =
    /// 命題論理の論理式を表す
    type PropLogicExpr =
        | Const of bool
        | Var of string
        | Or of PropLogicExpr list
        | And of PropLogicExpr list
    with
        // 標準形 (Or [] や Or [...True...]、Or [...Or [...]...] のような形を簡単にする)
        member this.NormalForm =
            match this with
            | Const _ | Var _ -> this
            | Or  cps -> cps |> PropLogicExpr.NormalFormOfOr
            | And cps -> cps |> PropLogicExpr.NormalFormOfAnd
            
        static member private NormalFormOfOr ps =
            let ps =
                (ps |> List.collect (fun p ->
                    match p.NormalForm with
                    | Or inner -> inner
                    | Const false -> []
                    | p -> [ p ] ))
                |> (fun ls -> if ls |> List.exists ((=) (Const true)) then [Const true] else ls)
                |> Seq.distinct |> List.ofSeq
            match ps with
            | [] -> Const false
            | [ p ] -> p.NormalForm
            | ps -> Or ps

        static member private NormalFormOfAnd ps =
            let ps =
                (ps |> List.collect (fun p ->
                    match p.NormalForm with
                    | And inner -> inner
                    | Const true -> []
                    | p -> [ p ] ))
                |> (fun ls -> if ls |> List.exists ((=) (Const false)) then [Const false] else ls)
                |> Seq.distinct |> List.ofSeq
            match ps with
            | [] -> Const true
            | [ p ] -> p.NormalForm
            | ps -> And ps
                
        // 選言標準形
        member this.DisjNormalForm =
            (this.DisjNormalForm' : PropLogicExpr).NormalForm
        member private this.DisjNormalForm' =
            match this with
            | Const _ | Var _ -> this
            | Or cps -> Or (cps |> List.map (fun cp -> cp.DisjNormalForm'))
            | And cps -> PropLogicExpr.DNF_AndCps cps

        // 分配律を使って And を内側に送り込む
        static member private DNF_AndCps cps =
            cps
            |> List.map (fun cp -> cp.DisjNormalForm')
            |> List.map (function
                | Or cpsOr -> cpsOr
                | cp -> [cp] )
            |> List.product
            |> List.map And
            |> Or
    ()
