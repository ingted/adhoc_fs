﻿module Program

open System
open System.IO
open MTG
open AboutMusicFiles
open AboutSomething
open Uedai.Utilities

[<EntryPoint>]
let main argv =
    let argv = lazy Console.ReadCommandLine argv

    MTG.Handmade.main ()
    //AboutMusicFiles.mainMusic argv

    //AboutDownload.doing()
    //AboutWixoss.Something.sample()
    //AboutMath.AboutMath.sample()
    //AboutSomething.DownloadCardDataFromSkyGalleonWiki.updateCsvItems @"D:/Docs/downloads/it.csv"

    //exit code
    0
