namespace AdhocFs

open System.ComponentModel.DataAnnotations

[<AutoOpen>]
module Types =
  type User(name: string) =
    let mutable id = 0

    new () = User(null)

    member val Id           = 0                 with get, set
    member val Name         = name              with get, set
    member val Profile      = (None: option<string>)    with get, set

  [<AllowNullLiteral>]
  type Tweet(userId: int, content: string) =
    // dummy constructor
    new () = Tweet(0, null)

    member val Id = 0 with get, set
    member val UserId = userId with get, set
    member val Content = content with get, set
