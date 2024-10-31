module RecordTupleRecursionTests

open System
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Schema
open Expecto
open Expecto.Flip
open TestTypes


[<Tests>]
let tests =
    testList "FSharp Records and Tuples recursion schema generation tests" [

        testCase "should create a schema with a record that is self recursive from within a tuple" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "RecursiveRecordWithTuple",
  "type": "object",
  "properties": {
    "RecursiveTuple": {
      "type": "array",
      "items": [
        {
          "type": "string"
        },
        {
          "oneOf": [
            {
              "$ref": "#"
            },
            {
              "type": "null"
            }
          ]
        }
      ],
      "additionalItems": false,
      "additionalProperties": false
    },
    "Value": {
      "type": "integer",
      "format": "int32"
    }
  },
  "required": [
    "Value",
    "RecursiveTuple"
  ]
}"""

            let data = { Value = 20; RecursiveTuple = "Hello", Some { Value = 11; RecursiveTuple = "World", None } }
            let schema = draft4FromObj data |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"Value":20,"RecursiveTuple":["Hello",{"Value":11,"RecursiveTuple":["World",null]}]}"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

            // NOTE: this is valid, but it we won't be able to deserialize it :(
            let minimalJson = """{"Value":20,"RecursiveTuple":["Hello",{"Value":11,"RecursiveTuple":["World"]}]}"""
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(minimalJson)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

            //let data2 = FSharpLuLikeSerializer.deserialize minimalJson
            //Expect.equal "" data data2
]