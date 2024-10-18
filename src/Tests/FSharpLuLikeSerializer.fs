module FSharpLuLikeSerializer

open System
open System.Text.Json.Serialization
open System.Text.Json

let fsharpSystemTextJsonOptions =
    JsonFSharpOptions
        // FsharpLu: https://github.com/microsoft/fsharplu/blob/main/FSharpLu.Json.md
        // has a very succinct method for serializing/deserializing DU types.
        // Quite similar to how you would write it by hand IMHO. This options
        // makes Fsharp.SystemTextJson act like FsharpLu. See the
        // Fsharp.SystemTextJson docs for more details:
        // https://github.com/Tarmil/FSharp.SystemTextJson/blob/master/docs/Customizing.md#initial-options
        .FSharpLuLike()
        .WithUnionUnwrapSingleCaseUnions()
        // Minimal lets `System.Text.Json` handle most types as default:
        // Options, Records, Lists, Sets, Maps.
        // `Fsharp.SystemTextJson` will handle DUs and Tuples.
        .WithTypes(JsonFSharpTypes.Minimal)

let jsonSerializationOptions =
    let options = fsharpSystemTextJsonOptions.ToJsonSerializerOptions()
    options.DefaultIgnoreCondition <- JsonIgnoreCondition.WhenWritingNull
    options

let inline serialize obj =
    JsonSerializer.Serialize(obj, jsonSerializationOptions)

let inline deserialize (jsonStr : string) : 'a =
    JsonSerializer.Deserialize(jsonStr, jsonSerializationOptions)

