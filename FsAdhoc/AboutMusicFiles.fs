namespace AboutMusicFiles

open System
open System.IO
open System.Text.RegularExpressions
open System.Linq
open Microsoft.FSharp
open Microsoft.FSharp.Reflection
open System.Reflection
open System.Collections.Generic

open Basis.Core
open System.Runtime.Serialization
open System.Runtime.Serialization.Json
open WMPLib
open iTunesLib
open Tsukikage.DllPInvoke.MP3Tag    // mp3infp_from_fsharp.dll

open Samples.FSharp.RegexTypeProvider   // Sample.RegexTypeProvider.dll

open Uedai.Utilities

module AboutMusicFiles =
    // using WMPLib
    module WmpUtil =
        module Wmp =
            let private app = WindowsMediaPlayerClass()
            app.autoStart <- false

            let Value = app
            let AllTracks   = lazy app.getAll()
            let TemporaryPL = lazy app.playlistCollection.getByName("#_").[0]

            let DateFormat = "yyyy/MM/dd HH:mm:ss"

        // Playlist
        type IWMPPlaylist with
            member this.ToSeq =
                seq { for i in 0..(this.count - 1) -> this.Item i }

        // Media 
        type IWMPMedia with
            member this.Item (attr : string) =
                this.getItemInfo attr
            member this.AcquisitionTime =
                DateTime.Parse (this.["AcquisitionTime"])
            member this.LastPlayedTime =
                match this.["UserLastPlayedTime"] with
                | null | ""
                    -> None
                | s -> Some (DateTime.Parse(s))
        ()

    open WmpUtil

    // using iTunesLib
    module iTunesUtil =
        let iTunes = lazy new iTunesLib.iTunesAppClass()
        
        // iTunesを起動して操作対象(通常はすべてのトラック)を選択してから実行すべし
        //let itsTracks = iTunes.SelectedTracks 
        //if itsTracks = null then failwith "no target tracks"

    open iTunesUtil

    //-------------------------------------------

    module Str =
        /// for ex, change "something(anno)" to "something".
        let removeFollowingBracket (bracketLeft, bracketRight) str =
            Regex.Replace(str, "^(.*)" + Regex.Escape(bracketLeft) + ".*" + Regex.Escape(bracketRight) + "$",
                (fun (m : Match) -> m.Groups.[1].Value))
    
    /// 歌詞データをファイルから取り出す
    let internal loadLyricsFromFile fileName =
        if File.Exists fileName then
            use file = File.OpenText fileName
            file.ReadToEnd() |> sprintf "\r\n%s\r\n"
        else
            raise(new Exception())
    
    [<DataContract>]
    type SongMetadata = {
        [<field: DataMember(Name="title")>]    Title : string
        [<field: DataMember(Name="composer")>] Composer : string
        [<field: DataMember(Name="writer")>]   Writer : string
        [<field: DataMember(Name="vocal")>]    Vocal : string
        [<field: DataMember(Name="track")>]    TrackNumber : string
        [<field: DataMember(Name="release")>]  ReleaseYear : string
    }
    type MyLyricsSection = private {
        Metadata : string
        Body : string
        Idx : int
    }

    let splitLyricsIntoSongBlocks sectionSymbol (lyrics : string) =
        let splittee = lyrics |> Str.splitBy ("\r\n" + sectionSymbol)
        [
            for i, block in splittee.[1..] |> Array.toSeq |> Seq.indexed do
                let metadata, body =
                    match block.IndexOf "\r\n\r\n" with
                    | p when p >= 0 -> block |> Str.splitAt p
                    | _ -> block, ""
                yield { Metadata = metadata; Body = body; Idx = i }
        ]

    /// メタデータテキストからメタデータを個別に取り出す
    /// 例：「作曲：xxxP\n歌：初音ミク<Sweet>」→ { Composer = "xxxP"; Vocal = "初音ミク" }
    // todo: 列記されているときに最後の < > しか取り除けていない。
    let songMetadataFromText =
        let extract'' albumData (g : Group) getter =
            if g.Success then g.Value else
                match albumData with
                | Some album -> album |> getter
                | None -> ""
        let extract' albumData g getter =
            extract'' albumData g getter |> Str.removeFollowingBracket ("<", ">")

        let extTitle       = new RegexTyped< @"^([^\r]*)">()
        let extComposer    = new RegexTyped< @"(?:.*?)作曲(?:.*?)：([^\r]*)">()
        let extWriter      = new RegexTyped< @"(?:.*?)作詞(?:.*?)：([^\r]*)">()
        let extVocal       = new RegexTyped< @"(?:.*?)歌(?:.*?)：([^\r]*)">()
        let extTrackNumber = new RegexTyped< @"#([0-9]+)">()
        let extReleaseYear = new RegexTyped< @"#\(Release:.*?(\d+)">()

        fun (albumData : SongMetadata option) metadataText ->
            let extract = extract' albumData
            {
                Title       = extract (metadataText |> extTitle      .Match)._1 (fun a -> a.Title)
                Composer    = extract (metadataText |> extComposer   .Match)._1 (fun a -> a.Composer)
                Writer      = extract (metadataText |> extWriter     .Match)._1 (fun a -> a.Writer)
                Vocal       = extract (metadataText |> extVocal      .Match)._1 (fun a -> a.Vocal)
                TrackNumber = extract (metadataText |> extTrackNumber.Match)._1 (fun a -> a.TrackNumber)
                ReleaseYear = extract (metadataText |> extReleaseYear.Match)._1 (fun a -> a.ReleaseYear)
            }
            
    (*
    // version of using reflection
    [<Sealed; AttributeUsage(AttributeTargets.Property, Inherited = false, AllowMultiple = false)>]
    type private ExtractableMetadataAttribute (pattern) =
        inherit Attribute()

        let reg = new Regex(pattern)
        member __.Extract input =
            let m = reg.Match input
            Option.if' (m.Success && m.Groups.Count >= 1) (fun() -> m.Groups.[1].Value)

    type SongMetadata = private {
        [<ExtractableMetadata(@"^([^\r]*)")                    >] Title : string
        [<ExtractableMetadata(@"(?:.*?)作曲(?:.*?)：([^\r]*)") >] Composer : string
        [<ExtractableMetadata(@"(?:.*?)作詞(?:.*?)：([^\r]*)") >] Writer : string
        [<ExtractableMetadata(@"(?:.*?)歌(?:.*?)：([^\r]*)")   >] Vocal : string
        [<ExtractableMetadata(@"#([0-9]+)")                    >] TrackNumber : string
    }
    let songMetadataFromText albumData metadataText =
        let recordFields =
            Reflection.FSharpType.GetRecordFields (
                typeof<SongMetadata>, bindingFlags = Reflection.BindingFlags.NonPublic
            )
        let fields = [|
            for field in recordFields do
                let attr = field.GetCustomAttribute<ExtractableMetadataAttribute>()
                yield match attr.Extract metadataText with
                        | Some x -> x :> obj
                        | None ->
                            match albumData with
                            | Some album -> field.GetValue(album)
                            | None -> "" :> obj
        |]
        Reflection.FSharpValue.MakeRecord (
            typeof<SongMetadata>,
            fields,
            bindingFlags = Reflection.BindingFlags.NonPublic
        ) :?> SongMetadata
        *)
        
    /// 歌詞テキストを曲ごとに分解し、そのそれぞれを「メタデータ部分」「歌詞部分」に分解する
    (*
        セクション開始記号 sectionSymbol ("＊" か "－") の直後にある空行までがメタデータ部分、残りは歌詞部分
        (コメント行は除去するべきだが、今回は使わないので気にしない)
    //*)
    let internal newSongsDataFromMyLyrics lyrics =
        let (|AlbumTrack|SingleTrack|) =
            let reg = new RegexTyped< @"^『(?<albumTitle>.*)』$">()
            fun title ->
                let m = reg.Match title
                if m.albumTitle.Success
                    then AlbumTrack m.albumTitle.Value
                    else SingleTrack title

        let rec f sectionSymbol albumData lyrics =
            let lyricsSections = lyrics |> splitLyricsIntoSongBlocks sectionSymbol
            [
                for lyricsSection in lyricsSections do
                    let metadata = songMetadataFromText albumData (lyricsSection.Metadata)
                    match metadata.Title with
                    | SingleTrack title ->
                        match albumData with
                        | Some album -> yield { metadata with TrackNumber = string (lyricsSection.Idx + 1) }
                        | None -> yield metadata
                    | AlbumTrack title ->
                        yield! lyricsSection.Body |> f "－"  (Some metadata)
            ]

        lyrics |> f "＊" None

    // 歌詞ファイルから曲メタデータを取得
    let internal loadSongDataFromLyrics lyricsPath =
        lyricsPath |> loadLyricsFromFile |> newSongsDataFromMyLyrics
        
    /// 曲データ配列の入出力
    // F# の list<> は serialize できない。
    let saveSongsData path (songsData : SongMetadata list) =
        assert (path |> Path.GetExtension |> Str.toLower |> (=) ".json")
        IO.File.WriteAllText(path, Serialize.serializeJson(songsData |> List.toArray), Text.Encoding.UTF8)

    let loadSongsData path =
        assert (path |> Path.GetExtension |> Str.toLower |> (=) ".json")
        IO.File.ReadAllText (path, Text.Encoding.UTF8) |> Serialize.deserializeJson<SongMetadata[]> |> Array.toList

    //-------------------------------------------

    // 歌詞ファイルから曲メタデータを取得し、保存する
    let LoadAndSaveSongDataFromLyrics lyricsPath savedPath =
        let songsData = loadSongDataFromLyrics lyricsPath
        printfn "total %d titles found from the lyric file." songsData.Length
        saveSongsData savedPath songsData
        songsData

    /// 動画ファイルを探して、それらをサブフォルダに移動させる
    // songData のリストを保存する
    let MoveMoviesOfSongsToSubDir (dirPath, subDirName, songsData) =
        let titles =
            songsData |> List.map (fun songData -> songData.Title)

        // サブディレクトリがなければ作っておく
        let subDir = Path.Combine(dirPath, subDirName)
        if Directory.Exists subDir |> not then
            Directory.CreateDirectory(subDir) |> ignore

        // タイトルに曲名を含む動画ファイルを、すべてサブディレクトリに移動させる
        let dir = Directory.GetFiles dirPath
        let dirFound =
            dir |> Array.filter (fun fileName ->
                titles |> List.exists (fileName.Contains)
            )
        dirFound |> Array.iter (fun fileName ->
            let dstName = Path.Combine( (fileName |> Path.GetDirectoryName), subDir, (fileName |> Path.GetFileName) )
            File.Move (fileName, dstName)
        )

        // おそらく対応する動画ファイルを発見できてないであろう曲名を列挙しておく
        titles |> List.iter (fun title ->
            if dirFound |> Array.exists (fun dir -> dir.Contains title) |> not then
                printfn "「%s」's movie file maybe not found." title
        )
        
    //-------------------------------------------

    let internal removeMeaninglessGenre (tagInfo : TagInfo) =
        if tagInfo.Genre |> (fun s -> Str.isNullOrEmpty s || (s = "Other") || (s = "ジャンル情報なし")) then
            tagInfo.Genre <- null

    let internal isMusicFileExt ext =
        let legalExts = ["au"; "avi"; "aif"; "iff"; "mov"; "mp3"; "mpg"; "m4a"; "wav"; "wma"; "wrk"]
        legalExts |> Seq.contains ext

    /// フォルダーにあるそれっぽいファイルの、タグとファイル名を書き換える
    let internal CommitTagsToFiles dirPath songsData =
        let fileNames = Directory.GetFiles dirPath
        
        let filterFiles songData (files: string[]) =
            files
            |> Array.filter (fun fileName ->
                  fileName.Contains (songData.Title))
            |> Console.AskWhichOne ("Which is the file of '" + songData.Title + "'?")
            |> (function
                | Some (_, fileName) -> Some fileName
                | None ->
                    printfn "File of '%s' isn't found or skipped." (songData.Title)
                    None
                )

        let commitTag songData fileName =
            try
                let album =
                    if songData.Composer = songData.Writer
                        then songData.Composer
                        else sprintf "c:%s/w:%s" songData.Composer songData.Writer

                let setTag (tagInfo: #TagInfo) =
                    tagInfo.Title <- songData.Title
                    tagInfo.Album <- album
                    tagInfo.Artist <- songData.Vocal
                    tagInfo.TrackNumber <- songData.TrackNumber
                    //tagInfo.ReleaseYear <- songData.ReleaseYear
                    tagInfo |> removeMeaninglessGenre
                    tagInfo.SaveUnicode()

                match fileName |> Path.GetExtension |> Str.toLower with
                | ".mp3" ->
                    MP3Infp.RemoveMP3Tag(fileName, MP3Infp.MP3TagType.ID3v1) |> ignore
                    MP3Infp.AddMP3Tag   (fileName, MP3Infp.MP3TagType.ID3v2) |> ignore
                    let tagInfo = MP3Infp.LoadTag<TagInfo.MP3_ID3v2> (fileName)
                    tagInfo.Composer <- songData.Composer
                    setTag tagInfo
                | ".m4a" ->
                    let tagInfo = MP3Infp.LoadTag<TagInfo.MP4> (fileName)
                    tagInfo.Composer <- songData.Composer
                    setTag tagInfo
                | _ -> setTag <| MP3Infp.LoadTag (fileName)

                // 長い曲名は手動で再登録する必要があるので警告する
                if songData.Title.Length >= 15 then
                    printfn "Maybe 「%s」's title is too long." songData.Title
                if album.Length >= 15 then
                    printfn "Maybe album name 「%s」 is too long." album

                // ファイル名を曲名にする
                let newFileName =
                    Path.Escape (konst "_") (songData.Title) + Path.GetExtension fileName
                File.Move (fileName, newFileName)

            with
                // mp3infp.dll の関数が ANSI なので、ファイルパスに変な文字が含まれていると失敗することがある
                | :? IO.FileNotFoundException as e ->
                    printfn "Unfound file is skipped:\n\tfilename: %s\nsongdata = %A" fileName songData
                | :? MP3Infp.Mp3infpException as e ->
                    printfn "Error while MP3 tag editting:\r\nfileName: %s\r\nmsg: %s" fileName (e.Message)
                | e ->
                    printfn "Unknown error: %s" (e.Message)

        songsData |> List.iter (fun songData ->
            fileNames
            |> filterFiles songData
            |> Option.iter (commitTag songData)
            )

    /// 曲名の順番を記録したファイルを出力する
    let internal SaveSortOfSongs fileName songsData =
        let buf = new Text.StringBuilder()
        songsData |> List.iter (fun song ->
            buf.AppendLine song.Title |> ignore
        )
        use file = File.CreateText fileName
        file.Write (buf.ToString())

    /// mp3 追加処理サポート
    (*
        操作条件：
            指定のフォルダ (downloads/mp3) に mp3 ファイルと
            $lyrics.txt を入れる。

        $lyrics.txt から、各曲に対する作曲者や歌唱者などの情報(メタデータ)を抽出して、
        それを元に、各サウンドファイルの MP3 タグを設定する。
        ただし、「作曲者」タグ等がいじれないので、作曲・作詞の情報はアルバムに書いて、後で手動で直す。
    //*)
    (*
    let MP3RegisterationSupport (dir, lyricsPath, pathToNewlyRegisteredSongNamesFile) =
        Environment.CurrentDirectory <- dir
        let songsData = loadSongDataFromLyrics lyricsPath
        songsData |> CommitTagsToFiles dir
        songsData |> SaveSortOfSongs pathToNewlyRegisteredSongNamesFile
    //*)
    let MP3RegisterationSupport (dir, songsDataPath) =
        Environment.CurrentDirectory <- dir
        let songsData = loadSongsData songsDataPath
        songsData |> CommitTagsToFiles dir

    //-------------------------------------------

    let ChangeTagEncoding filePath =
        match filePath |> Path.GetExtension |> Str.toLower with
        | ".mp3" ->
            let hasID3v1 = MP3Infp.ContainsMP3Tag (filePath, MP3Infp.MP3TagType.ID3v1)
            let hasID3v2 = MP3Infp.ContainsMP3Tag (filePath, MP3Infp.MP3TagType.ID3v2)

            // ID3v1 -> ID3v2 に移動
            if hasID3v1 && not hasID3v2 then
                MP3Infp.AddMP3Tag (filePath, MP3Infp.MP3TagType.ID3v2) |> ignore
                let tiV1 = MP3Infp.LoadTag<TagInfo.MP3_ID3v1> (filePath)
                let tiV2 = MP3Infp.LoadTag<TagInfo.MP3_ID3v2> (filePath)
                TagInfo.Copy (tiV1, tiV2)

                // ID3v1 タグにない情報を WMP ライブラリから取得
                let pl = Wmp.Value.mediaCollection.getByName (tiV2.Title)
                pl.ToSeq
                |> Console.AskWhichOne
                    (sprintf "Which file is of '%s' in %s?" tiV2.Title filePath)
                |> Option.iter (fun (i, track) ->
                    tiV2.Composer <- track.["Composer"]
                    )

                tiV2.SaveUnicode ()

            else if hasID3v2 then
                let tiV2 = MP3Infp.LoadTag<TagInfo.MP3_ID3v2> (filePath)
                tiV2.SaveUnicode ()

            // ID3v1 を削除
            // → WMP に新しい項目として認識されてしまうことがあるので、消すべきでない
            //if hasID3v1 then MP3Infp.RemoveMP3Tag (filePath, MP3Infp.MP3TagType.ID3v1) |> ignore

        | ".m4a" ->
            let tagInfo = MP3Infp.LoadTag<TagInfo.MP4> filePath
            tagInfo.SaveUnicode ()
        | ext ->
            printfn "Unknown ext '%s': File '%s'" ext filePath

    let ChangeTagEncodingAll path =
        let doneListFile = Path.Combine [| path; "$CTEA_done.txt" |]
        let errListFile  = Path.Combine [| path; "$CTEA_error.txt" |]
        File.AppendAllText (doneListFile, "") // Create if not exists

        let doneSet  =
            new HashSet<string>(File.ReadAllLines doneListFile)
        let errSet =
            new HashSet<string>()
        let doneCount = ref 0

        /// 未処理のファイル
        let files =
            Directory.EnumerateFiles (path, "*.m*", SearchOption.AllDirectories)
            |> Seq.filter (fun fileName -> doneSet.Contains fileName |> not)
        try
            files |> Seq.iter (fun filePath ->
                try
                    ChangeTagEncoding filePath
                    doneSet.Add (filePath) |> ignore
                    doneCount |> Ref.inc
                with
                    | _ -> errSet.Add (filePath) |> ignore
                )
        finally
            printfn "%d files processed successfully." (!doneCount)
            printfn "%d errors reported." (errSet.Count)
            if errSet.Count <> 0 then
                File.WriteAllLines (doneListFile, doneSet.ToArray () |> Array.map string)
                File.WriteAllLines (errListFile,  errSet.ToArray ()  |> Array.map string)

    //-------------------------------------------

    /// 指定された名前の最も新しいトラックを得る
    let NewestTrackNamed title =
        let pl = Wmp.Value.mediaCollection.getByName(title)

        match pl.count with
        | 0 ->
            Console.WriteLine("found not track named " + title)
            None
        | 1 ->
            Some (pl.[0])

        // 同名の曲が複数見つかった場合は、最新のものを選ぶ
        | cnt when cnt >= 2 ->
            let acquistionTimes =
                pl.ToSeq
                |> Seq.map (fun track -> track.AcquisitionTime)
                |> List.ofSeq
            let iNewest, _ =
                acquistionTimes |> List.maxWithIndex snd
            Some (pl.[iNewest])
        | _ -> failwith "unreachable"

    /// 新登録曲のプレイリストを生成する
    (*
        曲名のリストが改行区切りで書かれた、指定のテキストファイルを開き、
        そのリストと同じ順番で、WMPのテンポラリ・プレイリストに曲を追加する。
    //*)
    (*
    let MakePlaylistOfNewlyRegisteredSongs fileName =
        let addList = Wmp.TemporaryPL.Value.appendItem
        
        if File.Exists fileName then
            use file = File.OpenText fileName
            while not file.EndOfStream do
                let songName = file.ReadLine()
                let t = NewestTrackNamed songName
                t |> Option.iter addList
        else
            failwithf "File not found: %s" fileName
    //*)
    let MakePlaylistOfNewlyRegisteredSongs songsDataJson =
        let songsData = loadSongsData songsDataJson
        songsData |> List.iter (fun { Title = title } ->
            title
            |> NewestTrackNamed
            |> Option.iter (Wmp.TemporaryPL.Value.appendItem)
        )

    //-------------------------------------------

    /// wmp ライブラリのデータをテキストファイルに出力する
    (*
        完全にインポートできる形式ではない
    *)
    let ExportWmpLibraryAsText dstFileName =
        let buf = new Text.StringBuilder()

        for i in 0..(Wmp.AllTracks.Value.count - 1) do
            let track = Wmp.AllTracks.Value.[i]
            let (succeeded, playCount), playCountStr =
                let s = track.["UserPlayCount"]
                Int32.TryParse s, s
            
            if playCount > 0 then
                buf.AppendLine
                    ([|
                        track.name
                        track.sourceURL
                        playCountStr
                        track.["AcquistionTime"]
                        track.["UserLastPlayedTime"]
                    |] |> Str.join "\t")
                    |> ignore

        let text = buf.ToString()
        File.WriteAllText(dstFileName, text)

    //-------------------------------------------
    let RenameAlbumFiles dir =
        let titlesFilePath = Path.Combine [| dir; "titles.txt" |]
        let titles =
            if  File.Exists titlesFilePath then
                File.ReadAllLines (titlesFilePath)
            else
                printfn "タイトルのリストを入力してください。(#1 から)"
                Uedai.Utilities.Console.ReadLines() |> Array.ofSeq

        printfn "以下のトランザクションを実行してよいですか？ (y/n)"
        let transactions =
            Directory.GetFiles(dir, "*.mp3")
            |> Array.choose (fun fileName ->
                fileName
                |> Path.GetFileName
                |> Str.subTo 2 |> Int32.TryParse |> Option.trialResult
                |> Option.bind (fun num ->
                    Option.if' (1 <= num && num <= titles.Count()) (fun() ->
                        (num, titles.[num - 1])
                        ))
                |> Option.map (fun (num, title) ->
                    let tagInfo = MP3Infp.LoadTag (fileName)
                    assert (tagInfo.TrackNumber = string num)
                    let destFilePath =
                        Path.Combine(dir,
                            (Str.format "{0:D2}" [num]) + " " + title + ".mp3"
                            )
                    printfn "%s:\n  移動 %s\n  タイトル変更 %s -> %s" fileName destFilePath tagInfo.Title title
                    (fun() -> 
                        tagInfo.Title <- title
                        tagInfo.SaveUnicode()
                        File.Move(Path.Combine(dir, fileName), destFilePath)
                        )
                    )
                )
        if Console.ReadYesNo() then
            transactions |> Array.iter ((|>) ())
        ()

    //-------------------------------------------

    /// wmp のプレイリストを iTunes のプレイリストに複製する
    // プレイリストは同名で作成される。
    let CopyPlaylistFromWmpToITunes wmpPlaylistName =
        //let dateToday = DateTime.Now
        //let dateThreshold = dateToday.AddMonths(-1) // 1ヶ月前まで

        let wmpPls = Wmp.Value.playlistCollection.getByName wmpPlaylistName
        
        for i in 0..(wmpPls.count - 1) do
            let wmpPl = wmpPls.[i]

            let itsPl =
                iTunes.Value.CreatePlaylist wmpPl.name
                :?> IITUserPlaylist

            Console.WriteLine ("create new playlist named " + itsPl.Name + " in iTunes")

            for j in 0..(wmpPl.count - 1) do
                let wmpTrack = wmpPl.[j]
                let status = itsPl.AddFile (wmpTrack.sourceURL)
                
                Console.WriteLine ("add file " + wmpTrack.sourceURL)
            ()
        ()
    
    //-------------------------------------------
    ///音楽ファイル関連の処理のエントリーポイント
    let mainMusic (argv: string []) =
        let String_Today = DateTime.Now.ToString("yyyy-MM-dd")
        let Path_RegisteratingMusicFiles = @"D:/Docs/downloads/$musics"
        let Path_SourceMovies = @"D:/NicoCacheData"
        let Path_Music = @"D:/Docs/Music"
        let FileName_TemporaryLyrics = Path_RegisteratingMusicFiles + @"/$lyrics.txt"
        let FileName_NewlyRegisteredSongNames = Path_Music + @"/newly_registered_songs.txt"
        let FileName_NewlySongsDataJson = Path_Music + "/$newlySongsData.json"
        let FileName_WmpLibText = Path_Music + (sprintf @"/wmplib_bak(%s).txt" String_Today)

        let (bSuccess, iMode) =
            if argv.Length >= 1 then
                Int32.TryParse(argv.[0])
            else
                (false, 0)
        if not bSuccess then failwith "Must be given mode of 0-3."

        let argv' = argv |> List.ofArray |> List.tail
        match (iMode, argv') with
        | (0, [lyricsPath; moviesDir; jsonPath]) ->
            // 動画ファイル検索
            if (File.Exists lyricsPath |> not) || (Directory.Exists moviesDir |> not) then
                printfn "usage: this 0 lyricsPath moviesDir jsonPath"
                printfn "Given parameters are something wrong:"
                printfn " lyricsPath = '%s'" lyricsPath
                printfn " moviesDir = '%s'" moviesDir
            else
                let songsData = LoadAndSaveSongDataFromLyrics lyricsPath jsonPath
                MoveMoviesOfSongsToSubDir (moviesDir, String_Today, songsData)

        | (1, [musicDir; jsonPath]) ->
            // mp3 タグ編集処理
            MP3RegisterationSupport (musicDir, jsonPath)
            //MP3RegisterationSupport(Path_RegisteratingMusicFiles, FileName_NewlySongsDataJson)
        | (2, [jsonPath]) ->
            // 新曲プレイリスト生成
            MakePlaylistOfNewlyRegisteredSongs jsonPath
            //MakePlaylistOfNewlyRegisteredSongs FileName_NewlySongsDataJson
        | (3, [outPath]) ->
            ExportWmpLibraryAsText outPath
        | (4, [albumDir]) ->
            RenameAlbumFiles albumDir
        | (5, [dir]) ->
            ChangeTagEncodingAll dir
        | _ ->
            failwith "unsupported mode."
