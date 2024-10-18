module AttributeTests

open System
open System.Text.Json.Serialization

// TODO: Should create a schema using the JsonName attribute to set the name
//       for records properties, DUs fields, etc

type CustomerType =
    | [<JsonName "personal">] Personal
    | [<JsonName "business">] Business

// TODO: Should create a schema using the JsonPropertyName attribute to set the name
//       for records properties, DUs fields, etc

type RecordWithJsonPropertyNames =
    { [<JsonName("NameILikeEventMore")>]
      NameILike : string
      [<JsonPropertyName("SuperDuperName")>]
      NameIDontLike : string }


// TODO: Should create a schema that ignores fields with the JsonIgnore property

type RecordWithJsonIgnore =
    { IncludeMe : string
      [<JsonIgnore>]
      IgnoreMe : string }

// TODO: Should create a schema supporting the `JsonRequired` attribute

type RecordWithRequiredField =
    { NotRequired : Nullable<int>
      [<JsonRequired>]
      Required : Nullable<int> }

// TODO: Should create a schema supporting the `JsonNumberHandling` attribute

type RecordWithJsonNumberHandlingAttribute =
    { AsNumber : Decimal
      [<JsonNumberHandling(JsonNumberHandling.WriteAsString)>]
      WriteAsString : JsonNumberHandling
      [<JsonNumberHandling(JsonNumberHandling.AllowReadingFromString)>]
      ReadAsString : JsonNumberHandling
      [<JsonNumberHandling(JsonNumberHandling.WriteAsString &&& JsonNumberHandling.AllowReadingFromString)>]
      ReadAndWriteAsString : JsonNumberHandling }


// TODO: Should create a schema supporting the `JsonPropertyOrder` attribute

type RecordWithPropertyOrder =
    { [<JsonPropertyOrder(0)>]
      Second : string
      [<JsonPropertyOrder(1)>]
      First : string }

// TODO: Should create a schema supporting the `JsonKnownNamingPolicy` attribute

[<JsonSourceGenerationOptions(PropertyNamingPolicy = JsonKnownNamingPolicy.SnakeCaseLower)>]
type RecordWithNamingPolocies =
    { IncludeMe : string
      IgnoreMe : string }



