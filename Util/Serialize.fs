namespace Util

open System

module Serialize =
  open System.Runtime.Serialization
  open System.Runtime.Serialization.Json

  // https://gist.github.com/theburningmonk/2071722
  let private toString = System.Text.Encoding.UTF8.GetString
  let private toBytes (x : string) = System.Text.Encoding.UTF8.GetBytes x

  let serializeJson<'a> (x : 'a) = 
      let jsonSerializer = new DataContractJsonSerializer(typedefof<'a>)

      use stream = new IO.MemoryStream()
      jsonSerializer.WriteObject(stream, x)
      toString <| stream.ToArray()

  let deserializeJson<'a> (json : string) =
      let jsonSerializer = new DataContractJsonSerializer(typedefof<'a>)

      use stream = new IO.MemoryStream(toBytes json)
      jsonSerializer.ReadObject(stream) :?> 'a
