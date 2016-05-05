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

      Database.SetInitializer(SampleDbInitializer(mb))

  and SampleDbInitializer(mb) =
    inherit SqliteDropCreateDatabaseWhenModelChanges<SampleDbContext>(mb)

[<AutoOpen>]
module DatabaseExtension =
  type DbCtx = Database.SampleDbContext

  let withDb f =
    use ctx = new DbCtx()
    f ctx
