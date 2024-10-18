module FSharp.CrystalLake.JsonSerializer

open System
open Microsoft.FSharp.Reflection
open System.Text.Json.Serialization
open System.Text.Json
open Draft4

// I couldn't get Fsharp.SystemTextJson to serialize DUs without including the
// tag names. There is the `WithUnionUntagged` option, but unless I was making
// some type of mistake that didn't seem to do what I wanted
type CustomTypeFieldConverter() =
    inherit JsonConverter<TypeField>()

    override _.Write(writer, value, options) =
        match value with
        | Simple str -> writer.WriteStringValue str
        | Composite arr ->
            writer.WriteStartArray()
            arr |> Array.iter (fun x -> writer.WriteStringValue x)
            writer.WriteEndArray()

    override this.Read(reader, typeToConvert, options) = failwith "todo"

let fsharpSystemTextJsonOptions =
    JsonFSharpOptions
        // FsharpLu: https://github.com/microsoft/fsharplu/blob/main/FSharpLu.Json.md
        // has a very succinct method for serializing/deserializing DU types.
        // Quite similar to how you would write it by hand IMHO. This options
        // makes Fsharp.SystemTextJson act like FsharpLu. See the
        // Fsharp.SystemTextJson docs for more details:
        // https://github.com/Tarmil/FSharp.SystemTextJson/blob/master/docs/Customizing.md#initial-options
        .Default() //.FSharpLuLike()
        //.WithUnionUnwrapFieldlessTags()
        .WithUnionUntagged()
        //.WithUnionUnwrapRecordCases()
        //.WithUnionUnwrapSingleCaseUnions()
        // Minimal lets `System.Text.Json` handle most types as default:
        // Options, Records, Lists, Sets, Maps.
        // `Fsharp.SystemTextJson` will handle DUs and Tuples.
        .WithTypes(JsonFSharpTypes.Minimal)

let buildJsonSerializationOptions () =
    //let options =fsharpSystemTextJsonOptions.ToJsonSerializerOptions()
    let options = JsonSerializerOptions()
    options.Converters.Add(CustomTypeFieldConverter())
    fsharpSystemTextJsonOptions.AddToJsonSerializerOptions options
    options

let jsonSerializationOptions = buildJsonSerializationOptions ()

let inline serialize obj =
    JsonSerializer.Serialize(obj, jsonSerializationOptions)

let inline deserialize (jsonStr : string) : 'a =
    JsonSerializer.Deserialize(jsonStr, jsonSerializationOptions)

