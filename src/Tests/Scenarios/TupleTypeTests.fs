module TupleTypeTests

open System
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Schema
open Expecto
open Expecto.Flip
open TestTypes

[<Tests>]
let tests =
    testList "FSharp Tuples schema generation tests" [

        testCase "should create a simple tuple schema" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "TupleOfStringAndInt32AndBoolean",
  "type": "array",
  "items": [
    {
      "type": "string"
    },
    {
      "type": "integer",
      "format": "int32"
    },
    {
      "type": "boolean"
    }
  ],
  "additionalItems": false,
  "additionalProperties": false
}"""

            let data = "Hello", 42, false
            let schema = draft4FromObj data |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """["Hello",42,false]"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

        testCase "should create a simple optional tuple schema" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "TupleOfStringAndInt32AndBoolean",
  "type": [
    "array",
    "null"
  ],
  "items": [
    {
      "type": "string"
    },
    {
      "type": "integer",
      "format": "int32"
    },
    {
      "type": "boolean"
    }
  ],
  "additionalItems": false,
  "additionalProperties": false
}"""

            let data = Some ("Hello", 42, false)
            let schema = draft4FromObj data |> Helper.toJson
            Expect.equal "" expectedSchema schema

            //** With the value present **//
            // Check the data serializes as expected
            let expectedJson = """["Hello",42,false]"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

            //** With no value **//
            // Check the data serializes as expected
            let data : (string * int * bool) option = None
            let expectedJson = """null"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

        testCase "should create a simple tuple schema with optional items" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "TupleOfStringAndInt32AndBoolean",
  "type": "array",
  "items": [
    {
      "type": [
        "string",
        "null"
      ]
    },
    {
      "type": [
        "integer",
        "null"
      ],
      "format": "int32"
    },
    {
      "type": [
        "boolean",
        "null"
      ]
    }
  ],
  "additionalItems": false,
  "additionalProperties": false
}"""

            let data = Some "Hello", Some 42, Some false
            let schema = draft4FromObj data |> Helper.toJson
            Expect.equal "" expectedSchema schema

            //** With all values present **//
            // Check the data serializes as expected
            let expectedJson = """["Hello",42,false]"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

            //** With no values present **//
            // Check the data serializes as expected
            let data : string option * int option * bool option = None, None, None
            let expectedJson = """[null,null,null]"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))


        testCase "should create a simple tuple schema with nullable items" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "TupleOfInt32AndBoolean",
  "type": "array",
  "items": [
    {
      "type": [
        "integer",
        "null"
      ],
      "format": "int32"
    },
    {
      "type": [
        "boolean",
        "null"
      ]
    }
  ],
  "additionalItems": false,
  "additionalProperties": false
}"""

            let data : Nullable<int> * Nullable<bool> = Nullable(42), Nullable(false)
            let schema = draft4FromObj data |> Helper.toJson
            Expect.equal "" expectedSchema schema

            //** With all values present **//
            // Check the data serializes as expected
            let expectedJson = """[42,false]"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

            //** With no values present **//
            // Check the data serializes as expected
            let data : Nullable<int> * Nullable<bool> = Nullable<int>(), Nullable<bool>()
            let expectedJson = """[null,null]"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))


        testCase "should create a simple struct tuple schema" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "TupleOfSingleAndDateTimeOffsetAndString",
  "type": "array",
  "items": [
    {
      "type": "number",
      "format": "float"
    },
    {
      "type": "string",
      "format": "date-time"
    },
    {
      "type": "string"
    }
  ],
  "additionalItems": false,
  "additionalProperties": false
}"""

            let data = struct (55.823f, new DateTimeOffset(DateTime(2024,01,16,14,05,47)), "Lorem Ipsum")
            let schema = draft4FromObj data |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """[55.823,"2024-01-16T14:05:47+00:00","Lorem Ipsum"]"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))


        testCase "should create a tuple schema for nested tuples" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "TupleOfTupleOfBooleanAndInt64AndTupleOfStringAndDecimal",
  "type": "array",
  "items": [
    {
      "title": "TupleOfBooleanAndInt64",
      "type": "array",
      "items": [
        {
          "type": "boolean"
        },
        {
          "type": "integer",
          "format": "int64"
        }
      ],
      "additionalItems": false,
      "additionalProperties": false
    },
    {
      "title": "TupleOfStringAndDecimal",
      "type": "array",
      "items": [
        {
          "type": "string"
        },
        {
          "type": "number",
          "format": "decimal"
        }
      ],
      "additionalItems": false,
      "additionalProperties": false
    }
  ],
  "additionalItems": false,
  "additionalProperties": false
}"""

            let data = (true, 10L), ("Hello", 44.1234m)
            let schema = draft4FromObj data |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """[[true,10],["Hello",44.1234]]"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

        testCase "should create a tuple schema with a record type" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "TupleOfStringAndSimpleRecord",
  "type": "array",
  "items": [
    {
      "type": [
        "string",
        "null"
      ]
    },
    {
      "title": "SimpleRecord",
      "type": "object",
      "properties": {
        "A": {
          "type": "string"
        },
        "B": {
          "type": "integer",
          "format": "int32"
        },
        "C": {
          "type": "boolean"
        }
      },
      "required": [
        "A",
        "B",
        "C"
      ]
    }
  ],
  "additionalItems": false,
  "additionalProperties": false
}"""

            let data = Some "Hello", { A = "World"; B = 42; C = false }
            let schema = draft4FromObj data |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """["Hello",{"A":"World","B":42,"C":false}]"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))



        testCase "should create a tuple schema with record types inside a tuple" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "TupleOfStringAndTupleOfBooleanAndSimpleRecord",
  "type": "array",
  "items": [
    {
      "type": [
        "string",
        "null"
      ]
    },
    {
      "title": "TupleOfBooleanAndSimpleRecord",
      "type": "array",
      "items": [
        {
          "type": "boolean"
        },
        {
          "title": "SimpleRecord",
          "type": "object",
          "properties": {
            "A": {
              "type": "string"
            },
            "B": {
              "type": "integer",
              "format": "int32"
            },
            "C": {
              "type": "boolean"
            }
          },
          "required": [
            "A",
            "B",
            "C"
          ]
        }
      ],
      "additionalItems": false,
      "additionalProperties": false
    }
  ],
  "additionalItems": false,
  "additionalProperties": false
}"""

            let data = Some "Hello", (true, { A = "Hello"; B = 888; C = false })
            let schema = draft4FromObj data |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """["Hello",[true,{"A":"Hello","B":888,"C":false}]]"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

        testCase "should create a tuple schema with duplicate record types" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "TupleOfInt32AndSimpleRecordAndSimpleRecord",
  "type": "array",
  "items": [
    {
      "type": "integer",
      "format": "int32"
    },
    {
      "$ref": "#/definitions/SimpleRecord"
    },
    {
      "$ref": "#/definitions/SimpleRecord"
    }
  ],
  "additionalItems": false,
  "additionalProperties": false,
  "definitions": {
    "SimpleRecord": {
      "type": "object",
      "properties": {
        "A": {
          "type": "string"
        },
        "B": {
          "type": "integer",
          "format": "int32"
        },
        "C": {
          "type": "boolean"
        }
      },
      "required": [
        "A",
        "B",
        "C"
      ]
    }
  }
}"""

            let data = 123,
                       { A = "Hello"; B = 98; C = true },
                       { A = "World"; B = 1582; C = false }
            let schema = draft4FromObj data |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """[123,{"A":"Hello","B":98,"C":true},{"A":"World","B":1582,"C":false}]"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

        testCase "should create a tuple schema with duplicate record types across nested tuples" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "TupleOfInt32AndTupleOfInt32AndSimpleRecordAndTupleOfBooleanAndTupleOfInt32AndSimpleRecord",
  "type": "array",
  "items": [
    {
      "type": "integer",
      "format": "int32"
    },
    {
      "title": "TupleOfInt32AndSimpleRecord",
      "type": "array",
      "items": [
        {
          "type": "integer",
          "format": "int32"
        },
        {
          "$ref": "#/definitions/SimpleRecord"
        }
      ],
      "additionalItems": false,
      "additionalProperties": false
    },
    {
      "title": "TupleOfBooleanAndTupleOfInt32AndSimpleRecord",
      "type": "array",
      "items": [
        {
          "type": "boolean"
        },
        {
          "title": "TupleOfInt32AndSimpleRecord",
          "type": "array",
          "items": [
            {
              "type": "integer",
              "format": "int32"
            },
            {
              "$ref": "#/definitions/SimpleRecord"
            }
          ],
          "additionalItems": false,
          "additionalProperties": false
        }
      ],
      "additionalItems": false,
      "additionalProperties": false
    }
  ],
  "additionalItems": false,
  "additionalProperties": false,
  "definitions": {
    "SimpleRecord": {
      "type": "object",
      "properties": {
        "A": {
          "type": "string"
        },
        "B": {
          "type": "integer",
          "format": "int32"
        },
        "C": {
          "type": "boolean"
        }
      },
      "required": [
        "A",
        "B",
        "C"
      ]
    }
  }
}"""

            let data = 123,
                       ( 432, { A = "Hello"; B = 98; C = true } ),
                       ( false, ( 741, { A = "World"; B = 1582; C = false } ) )
            let schema = draft4FromObj data |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """[123,[432,{"A":"Hello","B":98,"C":true}],[false,[741,{"A":"World","B":1582,"C":false}]]]"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

        ////////////////////////////////////
        //** Optional Records in Tuples **//
        ////////////////////////////////////

        testCase "should create a tuple schema with an optional record type" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "TupleOfStringAndSimpleRecord",
  "type": "array",
  "items": [
    {
      "type": "string"
    },
    {
      "title": "SimpleRecord",
      "type": [
        "object",
        "null"
      ],
      "properties": {
        "A": {
          "type": "string"
        },
        "B": {
          "type": "integer",
          "format": "int32"
        },
        "C": {
          "type": "boolean"
        }
      },
      "required": [
        "A",
        "B",
        "C"
      ]
    }
  ],
  "additionalItems": false,
  "additionalProperties": false
}"""

            let data = "Hello", Some { A = "World"; B = 42; C = false }
            let schema = draft4FromObj data |> Helper.toJson
            Expect.equal "" expectedSchema schema

            //** With the record present **//
            // Check the data serializes as expected
            let expectedJson = """["Hello",{"A":"World","B":42,"C":false}]"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

            //** With the record missing **//
            let data : string * SimpleRecord option = "Hello", None
            // Check the data serializes as expected
            let expectedJson = """["Hello",null]"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

        testCase "should create a tuple schema with multiple different optional record types" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "TupleOfStringAndSimpleRecordAndSimpleRecord2",
  "type": "array",
  "items": [
    {
      "type": "string"
    },
    {
      "title": "SimpleRecord",
      "type": [
        "object",
        "null"
      ],
      "properties": {
        "A": {
          "type": "string"
        },
        "B": {
          "type": "integer",
          "format": "int32"
        },
        "C": {
          "type": "boolean"
        }
      },
      "required": [
        "A",
        "B",
        "C"
      ]
    },
    {
      "title": "SimpleRecord2",
      "type": [
        "object",
        "null"
      ],
      "properties": {
        "Alpha": {
          "type": "string",
          "format": "date-time"
        },
        "Beta": {
          "type": "number",
          "format": "decimal"
        }
      },
      "required": [
        "Alpha",
        "Beta"
      ]
    }
  ],
  "additionalItems": false,
  "additionalProperties": false
}"""

            let dt = DateTime(2024,09,16,16,48,18).ToUniversalTime()
            let data = "Hello", Some { A = "Hello"; B = 42; C = false }, Some { Alpha = dt; Beta = 5.731m }
            let schema = draft4FromObj data |> Helper.toJson
            Expect.equal "" expectedSchema schema

            //** With the record present **//
            // Check the data serializes as expected
            let expectedJson = """["Hello",{"A":"Hello","B":42,"C":false},{"Alpha":"2024-09-16T15:48:18Z","Beta":5.731}]"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

            //** With the record missing **//
            let data : string * SimpleRecord option * SimpleRecord2 option = "Hello", None, None
            // Check the data serializes as expected
            let expectedJson = """["Hello",null,null]"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

//        testCase "should create a tuple schema with duplicate optional record types" <| fun _ ->
//            let expectedSchema = """{
//  "$schema": "http://json-schema.org/draft-04/schema#",
//  "title": "TupleOfInt32AndSimpleRecordAndSimpleRecord",
//  "type": "array",
//  "items": [
//    {
//      "type": "integer",
//      "format": "int32"
//    },
//    {
//      "$ref": "#/definitions/SimpleRecord"
//    },
//    {
//      "$ref": "#/definitions/SimpleRecord"
//    }
//  ],
//  "additionalItems": false,
//  "additionalProperties": false,
//  "definitions": {
//    "SimpleRecord": {
//      "type": "object",
//      "properties": {
//        "A": {
//          "type": "string"
//        },
//        "B": {
//          "type": "integer",
//          "format": "int32"
//        },
//        "C": {
//          "type": "boolean"
//        }
//      },
//      "required": [
//        "A",
//        "B",
//        "C"
//      ]
//    }
//  }
//}"""
//
//            let data = 123,
//                       Some { A = "Hello"; B = 98; C = true },
//                       Some { A = "World"; B = 1582; C = false }
//            let schema = draft4FromObj data |> Helper.toJson
//            Expect.equal "" expectedSchema schema
//
//            //** With the records present **//
//            // Check the data serializes as expected
//            let expectedJson = """[123,{"A":"Hello","B":98,"C":true},{"A":"World","B":1582,"C":false}]"""
//            let json = FSharpLuLikeSerializer.serialize data
//            Expect.equal "" expectedJson json
//
//            // Check the serialized json matches the schema
//            let jSchema = JSchema.Parse(schema)
//            let jToken = JToken.Parse(json)
//            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))
//
//            //** With the records missing **//
//            let data : int * SimpleRecord option * SimpleRecord option = 123, None, None
//            // Check the data serializes as expected
//            let expectedJson = """[123,null,null]"""
//            let json = FSharpLuLikeSerializer.serialize data
//            Expect.equal "" expectedJson json
//
//            // Check the serialized json matches the schema
//            let jSchema = JSchema.Parse(schema)
//            let jToken = JToken.Parse(json)
//            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))


        // TODO: duplicate optional records types within different nestings

        ////////////////////////////////////
        //** Tuples within Records      **//
        ////////////////////////////////////


        testCase "should create a schema with a tuple inside a record" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "RecordWithTuple",
  "type": "object",
  "properties": {
    "Tuple": {
      "type": "array",
      "items": [
        {
          "type": "string"
        },
        {
          "type": "number",
          "format": "decimal"
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
    "Tuple"
  ]
}"""

            let data = { Value = 20; Tuple = "Hello", 22.12m }
            let schema = draft4FromObj data |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"Value":20,"Tuple":["Hello",22.12]}"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

        testCase "should create a schema with an optional tuple inside a record" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "RecordWithOptionalTuple",
  "type": "object",
  "properties": {
    "OptTuple": {
      "type": "array",
      "items": [
        {
          "type": "string"
        },
        {
          "type": "number",
          "format": "decimal"
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
    "Value"
  ]
}"""

            let data = { Value = 20; OptTuple = Some ("Hello", 22.12m) }
            let schema = draft4FromObj data |> Helper.toJson
            Expect.equal "" expectedSchema schema

            //** With the tuple present **//
            // Check the data serializes as expected
            let expectedJson = """{"Value":20,"OptTuple":["Hello",22.12]}"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

            //** With the tuple missing **//
            let data = { Value = 20; OptTuple = None }

            // Check the data serializes as expected
            let expectedJson = """{"Value":20}"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

        testCase "should create a schema with multiple different tuples inside a record" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "RecordWithMultipleTuples",
  "type": "object",
  "properties": {
    "TupleA": {
      "type": "array",
      "items": [
        {
          "type": "string",
          "format": "char"
        },
        {
          "type": "integer",
          "format": "uint8"
        }
      ],
      "additionalItems": false,
      "additionalProperties": false
    },
    "TupleB": {
      "type": "array",
      "items": [
        {
          "type": "string"
        },
        {
          "type": "number",
          "format": "decimal"
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
    "TupleA",
    "TupleB"
  ]
}"""

            let data = { Value = 20; TupleA = 's', 6uy ; TupleB = "Hello", 22.12m }
            let schema = draft4FromObj data |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"Value":20,"TupleA":["s",6],"TupleB":["Hello",22.12]}"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

        // Note: We could create a definition for the identical tuples, I'm ok with keeping them inline though
        testCase "should create a schema with multiple identical tuples inside a record" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "RecordWithMultipleIdenticalTuples",
  "type": "object",
  "properties": {
    "Tuple1a": {
      "type": "array",
      "items": [
        {
          "type": "string"
        },
        {
          "type": "number",
          "format": "decimal"
        }
      ],
      "additionalItems": false,
      "additionalProperties": false
    },
    "Tuple1b": {
      "type": "array",
      "items": [
        {
          "type": "string"
        },
        {
          "type": "number",
          "format": "decimal"
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
    "Tuple1a",
    "Tuple1b"
  ]
}"""

            let data = { Value = 20; Tuple1a = "Hello", -0.99218m ; Tuple1b = "World", 22.12m }
            let schema = draft4FromObj data |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"Value":20,"Tuple1a":["Hello",-0.99218],"Tuple1b":["World",22.12]}"""
            let json = FSharpLuLikeSerializer.serialize data
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

        ///////////////////////////////////////////
        //** Recursive Tuples (within records) **//
        ///////////////////////////////////////////

//        testCase "should create a schema with a record that is self recursive from within a tuple" <| fun _ ->
//            let expectedSchema = """{
//  "$schema": "http://json-schema.org/draft-04/schema#",
//  "title": "RecursiveRecordWithTuple",
//  "type": "object",
//  "properties": {
//    "Tuple1a": {
//      "type": "array",
//      "items": [
//        {
//          "type": "string"
//        },
//        {
//          "type": "number",
//          "format": "decimal"
//        }
//      ],
//      "additionalItems": false,
//      "additionalProperties": false
//    },
//    "Tuple1b": {
//      "type": "array",
//      "items": [
//        {
//          "type": "string"
//        },
//        {
//          "type": "number",
//          "format": "decimal"
//        }
//      ],
//      "additionalItems": false,
//      "additionalProperties": false
//    },
//    "Value": {
//      "type": "integer",
//      "format": "int32"
//    }
//  },
//  "required": [
//    "Value",
//    "Tuple1a",
//    "Tuple1b"
//  ]
//}"""
//
//            let data = { Value = 20; RecursiveTuple = "Hello", Some { Value = 11; RecursiveTuple = "World", None } }
//            let schema = draft4FromObj data |> Helper.toJson
//            Expect.equal "" expectedSchema schema
//
//            // Check the data serializes as expected
//            let expectedJson = """{"Value":20,"RecursiveTuple":["Hello",{"Value":11,"RecursiveTuple":["World",null]}]}"""
//            let json = FSharpLuLikeSerializer.serialize data
//            Expect.equal "" expectedJson json
//
//            // Check the serialized json matches the schema
//            let jSchema = JSchema.Parse(schema)
//            let jToken = JToken.Parse(json)
//            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))
//



// NOTE: Purely recursive Tuples aren't definable at runtime without generics:
//   type MyTuple = int * MyTuple option - will fail to compile
// and ditto for mutually recursive ones:
//   type MyTupleA = int * MyTupleB option
//   and  MyTupleB = string * MyTupleA option
// but we can make it recursive with generics:
//   type MyTuple<'a> = int * 'a option
//   let recursiveTuple : MyTuple<MyTuple<MyTuple<int>>> = 5, Some (7, Some (6, None))

// TODO: Remove titles from tuple value elements
// TODO: Tuples with Generic types
// TODO: Self Recursive record within tuple
// TODO: Self Recursive Root reference (on record) within tuple
// TODO: Tuples with Generic types and self recursion (but they're actually different types
// e.g. type MyTuple<'a> = int * 'a option
//      let recursiveTuple : MyTuple<MyTuple<MyTuple<int>>> = 5, Some (7, Some (6, None))
// TODO: Tuples with Generic types and multi-stage circular references



  ]
