namespace AdhocFs

open System.ComponentModel.DataAnnotations

[<AutoOpen>]
module Types =
  // レコードにはできないようだ
  [<AllowNullLiteral>]
  type User(name': string) =
    let mutable id = 0
    let mutable name = name'
    let mutable profile = (null: string)

    new () = User(null)

    member this.Id
      with get () = id
      and  set v  = id <- v

    member this.Name
      with get () = name
      and  set v  = name <- v

    member this.Profile
      with get () = profile
      and  set v  = profile <- v
