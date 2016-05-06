namespace AdhocFs

open System
open System.Data.Entity
open System.Linq
open SQLite.CodeFirst

module Database =
  type SampleDbContext() =
    inherit DbContext("sampleDb")

    override this.OnModelCreating(mb: DbModelBuilder) =
      // configure tables
      mb.Entity<User>() |> ignore
      mb.Entity<Tweet>() |> ignore

      Database.SetInitializer(SampleDbInitializer(mb))

  and SampleDbInitializer(mb) =
    inherit SqliteDropCreateDatabaseWhenModelChanges<SampleDbContext>(mb)

[<AutoOpen>]
module DatabaseExtension =
  type DbCtx = Database.SampleDbContext

  let withDb f =
    use ctx = new DbCtx()
    f ctx

module DbContext =
  let tryFind<'t when 't: null and 't: not struct> pred (ctx: DbCtx) =
    let (_: bool) = if true then true else pred (failwith "")
    ctx.Set<'t>().FirstOrDefault(pred) |> Option.ofObj

  let set<'t when 't: not struct> (ctx: DbCtx) =
    ctx.Set<'t>()
