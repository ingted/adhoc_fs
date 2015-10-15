// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open NUnit.Framework
open FsUnit

open Uedai.Utilities

module UtilitiesTest =
    let test() =
        let ls = Seq.iota 4
        let ``test iota`` =
            ls |> should equal [0; 1; 2; 3]
            Seq.iota 0 |> should equal []
        let ``test positive modulo`` =
            positiveModulo (-12) 7 |> should equal 2
        let ``test tryFirst`` =
            [] |> Seq.tryFirst |> should equal None
            ls |> Seq.tryFirst |> should equal (Some 0)
        let ``test tryLast`` =
            [] |> Seq.tryLast |> should equal None
            ls |> Seq.tryLast |> should equal (Some 3)
        let ``test tryNth`` =
            [] |> Seq.tryNth 0 |> should equal None
            ls |> Seq.tryNth (-1) |> should equal None
            ls |> Seq.tryNth 2 |> should equal (Some 2)
        let ``test indexed`` =
            [] |> Seq.indexed |> should equal []
            ls |> Seq.indexed |> should equal [0,0; 1,1; 2,2; 3,3]
        let ``test contains`` =
            [] |> Seq.contains 0 |> should equal false
            ls |> Seq.contains 2 |> should equal true
            ls |> Seq.contains 7 |> should equal false
        let ``test rotate`` =
            [] |> Seq.rotate 1 |> should equal []
            ls |> Seq.rotate 0 |> should equal ls
            ls |> Seq.rotate 1 |> should equal [1; 2; 3; 0]
            ls |> Seq.rotate (-2) |> should equal [2; 3; 0; 1]
        (*
        let ``test slice`` =
            //[] |> Seq.slice' (Some 1) (Some 3) |> should equal []
            ls |> Seq.slice' (Some 1) (Some 3) |> should equal [1; 2]
            ls |> Seq.slice' (None) (Some 3) |> should equal [0; 1; 2]
            ls |> Seq.slice' (Some 1) None |> should equal [1; 2; 3]
            ls |> Seq.slice' None None |> should equal ls
        //*)
             
        let ``test keys, values`` =
            let map = Map.ofList [0,'a'; 1,'b'; 2,'c'; 3,'a']
            map |> Map.keys |> should equal [0; 1; 2; 3]
            map |> Map.values |> should equal ['a'; 'b'; 'c'; 'a']
            map |> Map.inverse 'a' |> should equal [0; 3]
            //(map |> Map.toDictionary)

        let ``test MultiSet`` =
            let multiset1 = MultiSet.ofSeq [0; 1; 0; 2; 0; 1; 3]
            let multiset2 = MultiSet.ofSeq [0; 1; 2; 2; 4; 5]
            let multiset3 = MultiSet.ofSeq [0; 1; 1; 3]
            multiset1 |> MultiSet.count 1 |> should equal 2
            multiset1 |> MultiSet.count 4 |> should equal 0
            multiset1 |> MultiSet.contains 3 |> should equal true
            multiset1 |> MultiSet.union multiset2 |> MultiSet.toList |> should equal [0;0;0;0; 1;1;1; 2;2;2; 3;4;5]
            multiset1 |> MultiSet.intersect multiset2 |> MultiSet.toList |> should equal [0; 1; 2]
            multiset1 |> MultiSet.difference multiset2 |> MultiSet.toList |> should equal [0; 0; 1; 3]
            
            multiset1 |> MultiSet.isSubset multiset2 |> should equal false
            multiset1 |> MultiSet.isSuperset multiset2 |> should equal false
            multiset1 |> MultiSet.isSuperset multiset3 |> should equal true

        (*
         let ``test sortAsColorpie`` =
            (sortAsColorpie [White; Blue])  |> should equal [White; Blue]
            (sortAsColorpie [Blue; Black])  |> should equal [Blue; Black]
            (sortAsColorpie [White; Red])   |> should equal [Red; White]
            (sortAsColorpie [Blue; White; Black])   |> should equal [White; Blue; Black]       
            (sortAsColorpie [Red; White; Blue])     |> should equal [Red; White; Blue]
            (sortAsColorpie [Blue; Red; Green])     |> should equal [Red; Green; Blue]
            (sortAsColorpie [White; Blue; Red; Green])      |> should equal [Red; Green; White; Blue]
            (sortAsColorpie [Blue; Green; White; Black])    |> should equal [Green; White; Blue; Black]
            (sortAsColorpie [Red; Blue; White; Green; Black]) |> should equal [White; Blue; Black; Red; Green]
        //*)
        ()
    ()

module SkyGalleonWikiTest =
    open AboutSomething.DownloadCardDataFromSkyGalleonWiki
    let testEach (csv, cardName, rarity, status, nr, lr, gender, illust, spskill, midskill) =
        let source = tryDownloadSource cardName
        source.IsSome |> should equal true
        let source = source |> Option.get

        let ``404エラーでないことを確認する`` =
            source |> Str.tryIndexOf "は見つかりません" |> should equal None
        let ``csvからカード名を抽出する`` =
            csv |> cardNameFromCsv |> should equal cardName

        let ``レアリティを見つける`` =
            source |> tryFindRarity |> should equal (Some rarity)
        let ``枚数制限を見つける`` =
            source |> tryFindNumberRestriction rarity |> should equal (Some nr)
        let ``配置制限を見つける`` =
            source |> tryFindLocationRestriction |> should equal (Some lr)
        let ``分類を見つける`` =
            source |> tryFindGender |> should equal (Some gender)
        let ``絵師名を見つける`` =
            source |> tryFindIllustrator |> should equal (Some illust)

        let ``ステータスを見つける`` =
            let hp, at, ag = status
            source |> tryFindStatusValues rarity |> should equal [Some hp; Some at; Some ag]

        let ``特技を見つける`` =
            let skill' = source |> tryFindSkill "特技" rarity |> Option.get
            skill'.Effect1 |> Option.isSome |> should equal true
            let effect1' = skill'.Effect1 |> Option.get

            let gauge, prob, effect1, effect2opt = spskill
            let ``特技の発動設定を正しく見つける`` =
                skill'.Prob |> should equal (Some prob)
                skill'.Gauge |> should equal (Some gauge)

            let ``特技効果1`` =
                let color1, relationality1, side1, bits1 = effect1
                color1 |> should equal effect1'.Color
                let rangeInfo1 = formatRangeInfo (bits1, relationality1, side1)
                rangeInfo1 |> should equal effect1'.RangeInfo
                effect1'.CriticalPossibility |> should equal false

            let ``特技効果2`` =
                skill'.Effect2.IsSome |> should equal (effect2opt |> Option.isSome)
                match effect2opt, skill'.Effect2 with
                | Some effect2, Some effect2' ->
                    let color2,  relationality2, side2, bits2 = effect2
                    let rangeInfo2 = formatRangeInfo (bits2, relationality2, side2)
                    effect2'.Color |> should equal color2
                    effect2'.RangeInfo |> should equal rangeInfo2
                    effect2'.CriticalPossibility |> should equal false
                | _ -> ()
            ()

        let ``中列行動を見つける`` =
            let skill' = source |> tryFindSkill "中列" rarity |> Option.get
            skill'.Effect1 |> Option.isSome |> should equal true
            let eff1' =skill'.Effect1 |> Option.get

            let prob, effect1, effect2opt = midskill
            skill'.Gauge |> should equal None
            let ``効果1`` =
                let color1, relationality1, side1, bits1 = effect1
                eff1'.Color |> should equal color1
                let rangeInfo1 = formatRangeInfo (bits1, relationality1, side1)
                eff1'.RangeInfo |> should equal rangeInfo1
            ()
        ()

    let test() =
        let ``正しい攻撃係数を見つける`` =
            let triples =
                //ヘスティア中列より引用
                findAllCriticalCoeffTriples "R" [0.70;0.75;0.84;0.90;1.05;1.15]
            let baseCoeff = 
                triples |> List.map Tuple.fst'3 |> Seq.max
            assert(baseCoeff = 0.75)
        ()

        let ``optionlist extend`` =
            let ls1 = [None; Some 0; None; Some 1; None]
            let ls2 = [Some 1; None; None; Some 3]
            ls1 |> OptionList.extend 3 |> should equal [None; Some 0; None]
            ls1 |> OptionList.extend 7 |> should equal (ls1 @ [None; None])
            ls1 |> OptionList.merge ls2 |> should equal [Some 1; Some 0; None; Some 1; None]
            
        let examples =
            [ //旧テンプレ
                "ID,,7ET,SR+,無,SR+スサノオ,7,,,,護慧,,,,,,,,,,,,,,,,,,,,,,,",
                    "SR+スサノオ", "SR+", (140, 60, 11), ";", "前列", "男性", "い～どぅ～",
                    (5, 100, ("無", false, OppoSide, ((X,X,X),(X,X,X),(X,X,X))), Some ("無", true, NearSide, ((O,O,O),(O,X,O),(O,O,O)))),
                    (100, ("無", true, OppoSide, ((O,X,O),(O,O,O),(O,O,O))), None);
              //新テンプレ
              "ID,,6ET,R+,黒,R+ツィツィミトル,4,,,,;,,,,,,,,,,,,,,,,,,,,,,,,,,,,",
                    "R+ツィツィミトル", "R+", (120, 50, 39), ";", "前列;中列", "女性", "yapo",
                    (5, 100, ("無", true, NearSide, ((O,O,O),(O,X,O),(O,O,O))), Some ("黒", false, OppoSide, ((X,X,X),(X,X,X),(X,X,X)))),
                    (100, ("黒", false, OppoSide, ((O,O,O),(X,X,X),(X,X,X))), None);
            ]
        examples |> List.iter testEach
        ()

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    UtilitiesTest.test()
    SkyGalleonWikiTest.test()
    
    printfn "all tests completed successfully :)"
    0 // return an integer exit code
