namespace Botsu.MP3tag

// 翻訳
// Problem: TagInfo と MP3Infp の相互参照が解決できない
#if COMPILE
namespace Tsukikage.DllPInvoke.MP3Tag

// from tu3.jp/0834, github.com/ttsuki/ttsuki/blob/master/DllPInvoke/mp3infp.cs

open System
open System.Collections.Generic
open System.Text
open System.Runtime.InteropServices
open System.IO
open System.Reflection

module TagInfo =
    [<Sealed; global.System.AttributeUsage(AttributeTargets.Field, Inherited = true, AllowMultiple = false)>]
    type ValueNameAttribute (valueName : string) =
        inherit Attribute()

        let mutable _ValueName = valueName
        member this.ValueName
            with get() = _ValueName
            and private set value = _ValueName <- value

    [<AbstractClass>]
    type TagInfo =
            (*
        new (path : string) as this =
            this.Path <- Path.GetFullPath path
            //MP3Infp.ReloadTagInfo this
            *)

        // 注釈(上大)：C#版では setter は protected 指定になっているが、F# ではサポートされない。
        val mutable _Path : string
        member this.Path
            with get () = this._Path
            and internal set value = this._Path <- value

        [<ValueName("FILE")>]
        val mutable _FileName : string
        member this.FileName
            with get() = this._FileName
            and internal set value = this._FileName <- value
        
        [<ValueName("FEXT")>]
        val mutable _FileExt : string
        member this.FileExt
            with get() = this._FileExt
            and internal set value = this._FileExt <- value

        [<ValueName("SIZ1")>]
        val mutable _FileSize : string
        member this.FileSize
            with get() = this._FileSize
            and internal set value = this._FileSize <- value

        [<ValueName("VFMT")>]
        val mutable _VideoFormat : string
        member this.VideoFormat
            with get() = this._VideoFormat
            and internal set value = this._VideoFormat <- value

        [<ValueName("AFMT")>]
        val mutable _AudioFormat : string
        member this.AudioFormat
            with get() = this._AudioFormat
            and internal set value = this._AudioFormat <- value

        [<ValueName("TIME")>]
        val mutable _Duration : string
        member this.Duration
            with get() = this._Duration
            and internal set value = this._Duration <- value

        [<ValueName("INAM")>]
        val mutable public Title : string

        [<ValueName("IART")>]
        val mutable public Artist : string

        [<ValueName("IPRD")>]
        val mutable public Album : string

        [<ValueName("ICMT")>]
        val mutable public Comment : string

        [<ValueName("ICRD")>]
        val mutable public CreationDate : string

        [<ValueName("IGNR")>]
        val mutable public Genre : string

        [<ValueName("TRACK")>]
        val mutable public TrackNumber : string

        [<ValueName("ICOP")>]
        val mutable public Copyright : string

    [<AbstractClass>]
    type MP3ID3v1 =
        inherit TagInfo

        new (path : string) as this =
            { inherit TagInfo(path) }

module MP3Infp =
    /// メディアファイルの種類
    type MediaFileType =
        | Unknown = 0x00
        | MP3 = 0x01
        | WAV = 0x02
        | AVI = 0x03
        | VQF = 0x04
        | ASF = 0x05 // WMA, WMV, etc.
        | OGG = 0x07
        | APE = 0x08
        | MP4 = 0x09

    /// MP3フィルのタグの組み合わせを表す
    [<Flags>]
    type MP3TagType =
        | None = 0x0
        | ID3v1 = 0x1
        | ID3v2 = 0x2
        | RIFFSIF = 0x4
        | APE = 0x8
        

    /// APIの宣言
    module Native =
        // 同期用と思われる
        let SyncRoot = new obj()
        
        [<Flags>]
        type MP3TagType =
            | ID3V1 = 0x00000001
            | ID3V2 = 0x00000002
            | RIFFSIF = 0x00000004
            | ID3V1_0 = 0x00000008  // v2.43～
            | ID3V1_1 = 0x00000010  // v2.43～
            | ID3V2_2 = 0x00000020  // v2.43～
            | ID3V2_3 = 0x00000040  // v2.43～
            | ID3V2_4 = 0x00000080  // v2.43～
            | APEV1 = 0x00000100    // v2.47～
            | APEV2 = 0x00000200    // v2.47～
            
        [<DllImport("mp3infp.dll", EntryPoint = "mp3infp_GetVer", CharSet = CharSet.Ansi)>]
        extern int GetVer()
        
        [<DllImport("mp3infp.dll", EntryPoint = "mp3infp_ViewPropEx", CharSet = CharSet.Ansi, SetLastError = true)>]
        extern int ViewPropEx(IntPtr hWnd, [<MarshalAs(UnmanagedType.LPStr)>] string szFileName, int dwPage, [<MarshalAs(UnmanagedType.Bool)>] bool modeless, int param1, int param2)

        [<DllImport("mp3infp.dll", EntryPoint = "mp3infp_Load", CharSet = CharSet.Ansi)>]
        extern int Load(IntPtr hWnd, [<MarshalAs(UnmanagedType.LPStr)>] string szFileName)

        [<DllImport("mp3infp.dll", EntryPoint = "mp3infp_GetType", CharSet = CharSet.Ansi)>]
        extern MediaFileType GetFileType()

        [<DllImport("mp3infp.dll", EntryPoint = "mp3infp_GetValue", CharSet = CharSet.Ansi)>]
        extern [<return: MarshalAs(UnmanagedType.Bool)>] bool
            GetValue([<MarshalAs(UnmanagedType.LPStr)>] string szValueName, [<Out>] IntPtr buf)

        [<DllImport("mp3infp.dll", EntryPoint = "mp3infp_mp3_GetTagType", CharSet = CharSet.Ansi)>]
        extern MP3TagType GetTagType()

        [<DllImport("mp3infp.dll", EntryPoint = "mp3infp_SetConf", CharSet = CharSet.Ansi)>]
        extern int SetConf([<MarshalAs(UnmanagedType.LPStr)>] string tag, [<MarshalAs(UnmanagedType.LPStr)>] string value)

        [<DllImport("mp3infp.dll", EntryPoint = "mp3infp_SetValue", CharSet = CharSet.Ansi)>]
        extern int SetValue([<MarshalAs(UnmanagedType.LPStr)>] string szValueName, [<MarshalAs(UnmanagedType.LPStr)>] string buf)

        [<DllImport("mp3infp.dll", EntryPoint = "mp3infp_Save", CharSet = CharSet.Ansi)>]
        extern int Save([<MarshalAs(UnmanagedType.LPStr)>] string szFileName)

        [<DllImport("mp3infp.dll", EntryPoint = "mp3infp_mp3_MakeId3v1", CharSet = CharSet.Ansi)>]
        extern int MakeId3v1Tag([<MarshalAs(UnmanagedType.LPStr)>] string szFileName)

        [<DllImport("mp3infp.dll", EntryPoint = "mp3infp_mp3_DelId3v1", CharSet = CharSet.Ansi)>]
        extern int DeleteId3v1Tag([<MarshalAs(UnmanagedType.LPStr)>] string szFileName)

        [<DllImport("mp3infp.dll", EntryPoint = "mp3infp_mp3_MakeId3v2", CharSet = CharSet.Ansi)>]
        extern int MakeId3v2Tag([<MarshalAs(UnmanagedType.LPStr)>] string szFileName)

        [<DllImport("mp3infp.dll", EntryPoint = "mp3infp_mp3_DelId3v2", CharSet = CharSet.Ansi)>]
        extern int DeleteId3v2Tag([<MarshalAs(UnmanagedType.LPStr)>] string szFileName)

        [<DllImport("mp3infp.dll", EntryPoint = "mp3infp_mp3_MakeRMP", CharSet = CharSet.Ansi)>]
        extern int MakeRMPTag([<MarshalAs(UnmanagedType.LPStr)>] string szFileName)

        [<DllImport("mp3infp.dll", EntryPoint = "mp3infp_mp3_DelRMP", CharSet = CharSet.Ansi)>]
        extern int DeleteRMPTag([<MarshalAs(UnmanagedType.LPStr)>] string szFileName)

        [<DllImport("mp3infp.dll", EntryPoint = "mp3infp_mp3_MakeApeTag", CharSet = CharSet.Ansi)>]
        extern int MakeApeTag([<MarshalAs(UnmanagedType.LPStr)>] string szFileName)

        [<DllImport("mp3infp.dll", EntryPoint = "mp3infp_mp3_DelApeTag", CharSet = CharSet.Ansi)>]
        extern int DeleteApeTag([<MarshalAs(UnmanagedType.LPStr)>] string szFileName)

        ()

    /// MP3Infp が利用可能か否か
    let public Avaiable() =
        IO.File.Exists("mp3infp.dll")

    /// バージョン情報
    let public Version() =
        Native.GetVer()

    /// タグ情報を読み込む
    let LoadFile (path : string) =
        if not(File.Exists(path)) then
            raise (new FileNotFoundException("指定されたファイルが見つかりません", path))
        
        let path = Path.GetFullPath(path)
        lock (Native.SyncRoot) (fun () ->
            Native.Load(IntPtr.Zero, path)
        )

    /// ファイルの種類を判定する
    let GetFileType (path : string) =
        lock (Native.SyncRoot) (fun () ->
            LoadFile path
                |> ignore
            Native.GetFileType()
        )

    /// ファイルが指定の種類のファイルであるか否かを判定する
    let IsFileType (t : MediaFileType) (path : string) : bool =
        (GetFileType path) = t

    /// MP3ファイルに格納されているタグの種類を取得する
    let GetAvailableMP3TagType (path : string) : MP3TagType =
        let t = MP3TagType.None
        let tagType =
            lock (Native.SyncRoot) (fun () ->
                LoadFile path |> ignore
                Native.GetTagType()
            )
        in
            (if (tagType.HasFlag(Native.MP3TagType.ID3V1)) then MP3TagType.ID3v1 else MP3TagType.None)
        ||| (if (tagType.HasFlag(Native.MP3TagType.ID3V2)) then MP3TagType.ID3v2 else MP3TagType.None)
        ||| (if (tagType.HasFlag(Native.MP3TagType.RIFFSIF)) then MP3TagType.RIFFSIF else MP3TagType.None)
        ||| (if (tagType.HasFlag(Native.MP3TagType.APEV1)
                || tagType.HasFlag(Native.MP3TagType.APEV2)) then MP3TagType.ID3v1 else MP3TagType.None)

    /// ファイルが指定した種類のタグを含むかどうかを判断します
    let ContainsMP3Tag (t : MP3TagType) path =
        ((GetAvailableMP3TagType path) &&& t) = t

    /// 指定した形式のMP3タグを作成します。即座に反映します。
    let AddMP3Tag (t : MP3TagType) path =
        let path = Path.GetFullPath path
        
        lock (Native.SyncRoot) (fun () ->
            let err =
                LoadFile path |> ignore
                match t with
                | MP3TagType.ID3v1 -> Native.MakeId3v1Tag path
                | MP3TagType.ID3v2 -> Native.MakeId3v2Tag path
                | MP3TagType.RIFFSIF -> Native.MakeRMPTag path
                | MP3TagType.APE -> Native.MakeApeTag path
                | MP3TagType.None
                | _ -> -1   // some non-0 value
            if err > 0 then
                raise (new ComponentModel.Win32Exception(err))
            else
                err = 0
        )

    /// プロパティウィンドウを開く
    let OpenPropertyDialog (host : Windows.Forms.Form) (modeless : bool) path =
        let path = Path.GetFullPath path
        let r =
            Native.ViewPropEx ((if host != null then host.Handle else IntPtr.Zero),
                path, 0, modeless, 0, 0
            )
        if (r < 0) then
            raise (new ComponentModel.Win32Exception(Marshal.GetLastWin32Error()))

    /// 指定したファイルのタグを読み込む
    let LoadTag path =
        let path = Path.GetFullPath path

        match (GetFileType path) with
        | MediaFileType.MP3 ->
        (*
            (TagInfo)LoadTag<TagInfo.MP3_ID3v2>(path)
                        ?? (TagInfo)LoadTag<TagInfo.MP3_APE>(path)
                        ?? (TagInfo)LoadTag<TagInfo.MP3_RiffSIF>(path)
                        ?? (TagInfo)LoadTag<TagInfo.MP3_ID3v1>(path)
                        ?? (TagInfo)new TagImpl.Unknown(path)*)
        | MediaFileType.WAV -> LoadTag<TagInfo.WAV>(path)
        | MediaFileType.AVI -> LoadTag<TagInfo.AVI>(path)
        | MediaFileType.VQF -> LoadTag<TagInfo.VQF>(path)
        | MediaFileType.ASF -> LoadTag<TagInfo.ASF>(path)
        | MediaFileType.OGG -> LoadTag<TagInfo.OGG>(path)
        | MediaFileType.APE -> LoadTag<TagInfo.APE>(path)
        | MediaFileType.MP4 -> LoadTag<TagInfo.MP4>(path)
        | _ ->
            new TagImpl.Unknown(path)

#endif
