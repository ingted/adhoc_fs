// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

module Program

open System
open System.IO

open Basis.Core
open Basis.Core.Option

open AboutMusicFiles
open AboutMtg
open AboutSomething
open Uedai.Utilities

open FParsec

[<EntryPoint>]
let main argv =
    let argv = Console.ReadCommandLine argv

    //AboutMusicFiles.mainMusic argv

    //AboutDownload.doing()
    //AboutWixoss.Something.sample()
    //AboutMath.AboutMath.sample()
    //AboutSomething.DownloadCardDataFromSkyGalleonWiki.updateCsvItems @"D:/Docs/downloads/it.csv"

    //exit code
    0
