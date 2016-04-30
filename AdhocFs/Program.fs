open System

open FSharp.Data.Sql

type sql =
  SqlDataProvider<
      Common.DatabaseProviderTypes.MYSQL
    , ConnectionString  = "Data Source=localhost; user=root; password=;"
    , ResolutionPath    = @"C:\Program Files (x86)\MySQL\MySQL Connector Net 6.9.8\Assemblies\v4.0"
    , UseOptionTypes    = true
    , Owner             = "dyxi_muse"  // The name of the database
    >

(*
table people:
   id: int(11) AUTO INCREMENT, PRIMARY
   name: text (utf8_unicode_ci)
*)

let ctx = sql.GetDataContext()
let addPeople name =
  let people = ctx.DyxiMuse.Peoples.Create()
  people.Name <- name
  ctx.SubmitUpdates()

let printPeoples () =
  let q =
    query {
      for people in sql.GetDataContext().DyxiMuse.Peoples do
        select (people.Id, people.Name)
    }
  for (id, name) in q |> Seq.toArray do
    printfn "%d %s" id name

[<EntryPoint>]
let main argv =
  for i in 0..2 do
    addPeople (string i)

  // exit code
  0
