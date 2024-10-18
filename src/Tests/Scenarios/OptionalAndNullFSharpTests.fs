module OptionalAndNullFSharpTests

open System
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Schema
open Expecto
open Expecto.Flip
open TestTypes

[<Tests>]
let tests =
    testList "Optional and Null FSharp schema generation tests" [

        testCase "should create schema for a simple value option" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "Int32",
  "type": [
    "integer",
    "null"
  ],
  "format": "int32"
}"""
            let value : int Option = Some 42
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            //** For a Some value **//
            // Check the data serializes as expected
            let expectedJson = "42"
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

            //** For a None value **//
            // Check the data serializes as expected
            let noneValue : int Option = None
            let expectedJson = "null"
            let json = FSharpLuLikeSerializer.serialize noneValue
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

        testCase "should create a schema for simple nullable value" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "Int32",
  "type": [
    "integer",
    "null"
  ],
  "format": "int32"
}"""
            let value : Nullable<int> = Nullable<int>(42)
            let schema = draft4FromObj value |> Helper.toJson
            Expect.equal "" expectedSchema schema

            //** For a Some value **//
            // Check the data serializes as expected
            let expectedJson = "42"
            let json = FSharpLuLikeSerializer.serialize value
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

            //** For a None value **//
            // Check the data serializes as expected
            let noneValue : int Option = None
            let expectedJson = "null"
            let json = FSharpLuLikeSerializer.serialize noneValue
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

            // TODO - double optional should be flattened

    ]
