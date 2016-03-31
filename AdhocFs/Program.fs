open System
open FsYaml

//(*
type Node<'T> =
  {
    Value   : 'T
    Node    : list<Node<'T>>
  }
(* )

type Node =
  {
    Value : int
    Node : list<Node>
  }
  //*)

[<EntryPoint>]
let main argv =

  let node =
    {
      Value = 1
      Node =
        [
          {
            Value = 2
            Node = []
          }
        ]
    }

  let s = Yaml.dump(node)
  printfn "%s" s

  let n = Yaml.tryLoad<Node<int>> s

  // exit code
  0
