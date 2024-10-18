module ValueTypeTests

open System
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Schema
open Expecto
open Expecto.Flip
open TestTypes

// Note: https://docs.json-everything.net/schema/basics/ supports validation
// for draft 6 and later

// https://github.com/lateapexearlyspeed/Lateapexearlyspeed.JsonSchema supports
// validation for 2020.12 only

// NewtonSoft supports draft 6 and earlier

[<Tests>]
let tests =
    testList "Value type json schema generation tests" [
        testCase "should create a simple string model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "String",
  "type": "string"
}"""
            let value = "hello"
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "\"hello\""
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple boolean model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "Boolean",
  "type": "boolean"
}"""
            let value = false
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "false"
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple UUID model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "Guid",
  "type": "string",
  "format": "uuid"
}"""
            let value = Guid("0e5be31a-c791-4315-9672-f72c78ab8809")
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "\"0e5be31a-c791-4315-9672-f72c78ab8809\""
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple char model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "Char",
  "type": "string",
  "format": "char"
}"""
            let value = 'x'
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "\"x\""
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple int64 model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "Int64",
  "type": "integer",
  "format": "int64"
}"""
            let value = 123456789123456789L
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "123456789123456789"
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple uint64 model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "UInt64",
  "type": "integer",
  "format": "uint64"
}"""
            let value = 123456789123456789UL
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "123456789123456789"
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple int32 model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "Int32",
  "type": "integer",
  "format": "int32"
}"""
            let value = 123456789
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "123456789"
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple uint32 model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "UInt32",
  "type": "integer",
  "format": "uint32"
}"""
            let value = 123456789u
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "123456789"
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple int16 model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "Int16",
  "type": "integer",
  "format": "int16"
}"""
            let value = 12345s
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "12345"
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple uint16 model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "UInt16",
  "type": "integer",
  "format": "uint16"
}"""
            let value = 12345us
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "12345"
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple int8 model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "SByte",
  "type": "integer",
  "format": "int8"
}"""
            let value = 123y
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "123"
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple uint8 model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "Byte",
  "type": "integer",
  "format": "uint8"
}"""
            let value = 123uy
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "123"
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple double model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "Double",
  "type": "number",
  "format": "double"
}"""
            let value = 123456789.1234567
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "123456789.1234567"
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple float model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "Single",
  "type": "number",
  "format": "float"
}"""
            let value = 1234.1234f
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "1234.1234"
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple decimal model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "Decimal",
  "type": "number",
  "format": "decimal"
}"""
            let value = 123456789.1234567m
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "123456789.1234567"
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple DateTime model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "DateTime",
  "type": "string",
  "format": "date-time"
}"""
            let value = DateTime(2024,09,16,16,48,18).ToUniversalTime()
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "\"2024-09-16T15:48:18Z\""
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple DateTimeOffset model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "DateTimeOffset",
  "type": "string",
  "format": "date-time"
}"""
            let value = DateTimeOffset(DateTime(2024,09,16,16,48,18), TimeSpan.FromHours(5.0))
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "\"2024-09-16T16:48:18+05:00\""
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple DateOnly model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "DateOnly",
  "type": "string",
  "format": "date"
}"""
            let value = DateOnly.FromDateTime(DateTime(2024,09,16,16,48,18).ToUniversalTime())
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "\"2024-09-16\""
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a simple TimeOnly model" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "TimeOnly",
  "type": "string",
  "format": "time"
}"""
            let value = TimeOnly.FromDateTime(DateTime(2024,09,16,16,48,18).ToUniversalTime())
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = "\"15:48:18\""
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))
    ]
