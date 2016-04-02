open System

(*
open Util.Collections
open Util.Control
open Basis.Core

[<EntryPoint>]
let main argv =

  let x =
    update {
      //for i in 1..5 do
      do! Writer.write 1
      return ""//! Writer.write 1
    } |> Writer.writeRun
    |> printfn "%A"
//*)

open FSharp.Data.Sql

type sql =
  SqlDataProvider<
      Common.DatabaseProviderTypes.MYSQL
    , ConnectionString  = "Data Source=localhost; Port=3306; User ID=root; Password=root;"
    , ResolutionPath    = @"D:/repo/adhoc_fs/packages/MySQL Connector Net 6.9.8/Assemblies/v4.5"
    , IndividualsAmount = 100
    , UseOptionTypes    = true
    >

[<EntryPoint>]
let main argv =

  // exit code
  0
