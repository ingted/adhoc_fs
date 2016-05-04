namespace AdhocFs

open System.ComponentModel.DataAnnotations

[<AutoOpen>]
module Types =
  // レコードにはできないようだ
  type User(name: string) =
    let mutable id = 0
    let mutable profile = (null: string)

    new () = User(null)

    member this.Id
      with get () = id
      and  set v  = id <- v

    member this.Name
      with get () = name
      and  set (_: string) = ()//failwith "invalid operation"

    member this.Profile
      with get () = profile
      and  set v  = profile <- v
