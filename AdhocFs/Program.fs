namespace AdhocFs

open System
open System.Linq

module Option =
  let getOr x =
    function
    | Some x -> x
    | None -> x

  /// Returns Some x if the given x is NOT null; otherwise None.
  /// Unlike Optino.ofObj, you can apply this to values of an type for which F# doesn't allow null.
  let ofObj' (x: 'x) =
    if box x = null
    then None
    else Some x

module Program =
  let insertSample (ctx: DbCtx) =
    use transaction = ctx.Database.BeginTransaction()
    try
      let users =
        [|
          User("vain0")
          User("ue_dai", Profile = Some "My subaccount")
        |]
      let users2 = ctx.Set<User>().AddRange(users)
      ctx.SaveChanges() |> ignore

      let tweets =
        [|
          Tweet(users.[0].Id, "first tweet")
          Tweet(users.[0].Id, "second tweet")
        |]
      ctx.Set<Tweet>().AddRange(tweets) |> ignore
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
        (user.Profile |> Option.getOr "no profile")

  let findSample (ctx: DbCtx) userName =
    let userOpt =
      (ctx |> DbContext.set<User>).FirstOrDefault(fun user -> user.Name = userName) |> Option.ofObj'
      //ctx |> DbContext.tryFind<User> (fun user -> user.Name = userName)
    match userOpt with
    | Some user -> printUser user
    | None -> printfn "%s" "No user found."

  let querySample (ctx: DbCtx) =
    query {
      for tweet in ctx.Set<Tweet>() do
      join user in ctx.Set<User>() on (tweet.UserId = user.Id)
      select (user, tweet)
    }
    |> Seq.toArray
    |> Array.iter (fun (user, tweet) -> printfn "@%s: %s" user.Name tweet.Content)

  let printUsers (ctx: DbCtx) =
    for user in ctx.Set<User>() do
      printUser user

  [<EntryPoint>]
  let main argv = 
    withDb (fun ctx ->
      insertSample ctx
      //findSample ctx "vain0"
      //findSample ctx "vain0x"
      querySample ctx
      printUsers ctx
      )

    // exit code
    0
