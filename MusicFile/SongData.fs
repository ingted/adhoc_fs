namespace MusicFile

open System
open System.IO
open System.Text.RegularExpressions
open System.Runtime.Serialization
open System.Runtime.Serialization.Json

open Samples.FSharp.RegexTypeProvider   // Sample.RegexTypeProvider.dll

open Basis.Core
open Util
open Util.Collections

module Str =
  let removeFollowingBracket (bracketLeft, bracketRight) str =
    Regex.Replace(str, "^(.*)" + Regex.Escape(bracketLeft) + ".*" + Regex.Escape(bracketRight) + "$",
      (fun (m : Match) -> m.Groups.[1].Value))

module SongData =
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

  let splitLyricsIntoSongBlocks sectionSymbol (lyrics: string) =
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
    let extract'' albumData (g: Group) getter =
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

    fun (albumData: SongMetadata option) metadataText ->
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
  // reflection version

  open Microsoft.FSharp.Reflection
  [<Sealed; AttributeUsage(AttributeTargets.Property, Inherited = false, AllowMultiple = false)>]
  type private ExtractableMetadataAttribute (pattern) =
      inherit Attribute()

      let reg = new Regex(pattern)
      member __.Extract input =
          let m = reg.Match input
          Option.if' (m.Success && m.Groups.Count >= 1) (fun() -> m.Groups.[1].Value)

  type SongMetadata =
    private
    {
      [<ExtractableMetadata(@"^([^\r]*)")                    >] Title : string
      [<ExtractableMetadata(@"(?:.*?)作曲(?:.*?)：([^\r]*)") >] Composer : string
      [<ExtractableMetadata(@"(?:.*?)作詞(?:.*?)：([^\r]*)") >] Writer : string
      [<ExtractableMetadata(@"(?:.*?)歌(?:.*?)：([^\r]*)")   >] Vocal : string
      [<ExtractableMetadata(@"#([0-9]+)")                    >] TrackNumber : string
    }
  let songMetadataFromText albumData metadataText =
    let recordFields =
        Reflection.FSharpType.GetRecordFields
          (typeof<SongMetadata>
          , bindingFlags = Reflection.BindingFlags.NonPublic
          )
    let fields =
      [|
        for field in recordFields do
          let attr = field.GetCustomAttribute<ExtractableMetadataAttribute>()
          yield
            match attr.Extract metadataText with
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
    File.WriteAllText(path, Serialize.serializeJson(songsData |> List.toArray), Text.Encoding.UTF8)

  let loadSongsData path =
    assert (path |> Path.GetExtension |> Str.toLower |> (=) ".json")
    File.ReadAllText (path, Text.Encoding.UTF8) |> Serialize.deserializeJson<SongMetadata[]> |> Array.toList
