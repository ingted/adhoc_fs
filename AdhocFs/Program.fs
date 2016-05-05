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
      //transaction.Rollback()
      transaction.Commit()
      ()
    with
    | e ->
        transaction.Rollback()
        eprintfn "%s" e.Message

  let printUser (user: User) =
      printfn "#%02d '%s': %s"
        user.Id
        user.Name
        (if user.Profile = null then "no profile" else user.Profile)

  let findSample (ctx: DbCtx) userName =
    let userOpt =
      ctx.Set<User>().FirstOrDefault(fun user -> user.Name = userName) |> Option.ofObj
      //ctx |> DbContext.tryFind<User> (fun user -> user.Name = userName)
    match userOpt with
    | Some user -> printUser user
    | None -> printfn "%s" "No user found."

  let printUsers (ctx: DbCtx) =
    for user in ctx.Set<User>() do
      printUser user

  [<EntryPoint>]
  let main argv = 
    withDb (fun ctx ->
      //insertSample ctx
      findSample ctx "vain0"
      printUsers ctx
      )

    // exit code
    0
