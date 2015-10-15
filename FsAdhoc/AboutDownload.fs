module AboutDownload

open System
open System.Net
open System.Threading

let pathDownload = "D:/Docs/downloads"
let rnd = new Random()

let intervalAtRandom() =
    let r = 3000000
    rnd.Next r
let sleepAtRandom () =
    Thread.Sleep (intervalAtRandom())

let doing() =
    use wc = new WebClient()

    let downloadWixoss () =
        (*
        カードリストの処理

        カードリストHTMLから、収録番号 (WDX-NNN / WXMM-NNN)、固有ID (card_detail?id=MMM)、カード名、の3つ組のリストを作る。
        *)
        let htmlCardlist = wc.DownloadString "http://www.takaratomy.co.jp/products/wixoss/card/card_list.php?product_id=2"

        
        ()
    
    (*
    wc.DownloadFile("http://mtgwiki.com/index.php?title=%E3%82%B0%E3%83%A9%E3%83%B3%E3%83%97%E3%83%AA",
        pathDownload + "/groundprix.html")
    //wc.DownloadFile("http://www.wisdom-guild.net/image/title.png", PathDownload + "/test.png")
    //*)
    ()

