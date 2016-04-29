namespace AdhocFs

open System
open FSharp.Data

module Program =
  [<Literal>]
  let connectionString = @"Server=localhost;Database=test;Uid=root;Pwd=;"

  [<EntryPoint>]
  let main argv =
    use cmd =
      new SqlCommandProvider<"
        select * from users", @"Server=localhost;Database=test;Uid=root;Pwd=;">()



    // exit code
    0
