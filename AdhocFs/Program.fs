namespace AdhocFs

open System
open System.Linq

module Program =
  let insertSample (ctx: DbCtx) =
    use transaction = ctx.Database.BeginTransaction()
    try
      let users =
        [
          User("vain0")
          User("ue_dai", Profile = "My subaccount")
        ]
      let users2 = ctx.Set<User>().AddRange(users)
      ctx.SaveChanges() |> ignore
      transaction.Rollback()
      //transaction.Commit()
      ()
    with
    | e ->
        transaction.Rollback()
        eprintfn "%s" e.Message

  let printUsers (ctx: DbCtx) =
    for user in ctx.Set<User>() do
      printfn "#%02d '%s': %s"
        user.Id
        user.Name
        (if user.Profile = null then "no profile" else user.Profile)

  [<EntryPoint>]
  let main argv = 
    withDb (fun ctx ->
      insertSample ctx
      printUsers ctx
      )

    // exit code
    0
