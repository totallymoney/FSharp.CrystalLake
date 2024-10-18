module Helper

open System.Text.Json
open System.Text.Json.Serialization
open FSharp.CrystalLake

let toJson obj =
    let options = JsonSerializer.buildJsonSerializationOptions ()
    options.WriteIndented <- true
    options.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    JsonSerializer.Serialize(obj, options)

