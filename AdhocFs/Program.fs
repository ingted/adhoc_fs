open System
open FsYaml

type Node<'T> =  { Value: 'T; Node: list<Node<'T>> }
let node = { Value = 1; Node = [ { Value = 2; Node = [] } ] }
let yaml = Yaml.dump(node)
(*
&424836916 Node:
- Node: []
  &284240186 Value: 2
Value: 1
*)

[<EntryPoint>]
let main argv =


  // &数字.. を除去したもの
  let yaml = """
Node:
- Node: []
  Value: 2
Value: 1
"""

  // exit code
  0
