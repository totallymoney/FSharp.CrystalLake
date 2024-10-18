module RecordTypeTests

open System
open Newtonsoft.Json.Linq
open Newtonsoft.Json.Schema
open Expecto
open Expecto.Flip
open TestTypes

[<Tests>]
let tests =
    testList "FSharp Record schema generation tests" [
        testCase "should create a simple record schema" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
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
}"""

            let record = { A = "Hello"; B = 42; C = false }
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"A":"Hello","B":42,"C":false}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))

        testCase "should create a schema for an optional record at the root" <| fun _ ->
            // NOTE: we still list `required` properties even though it's
            // possible the result could be null. That's how json-schema works,
            // the properties and required are ignored if the type isn't object.
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
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
}"""

            let record = Some { A = "Hello"; B = 42; C = false }
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            //** With the record present **//
            // Check the data serializes as expected
            let expectedJson = """{"A":"Hello","B":42,"C":false}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

            //** With the record missing **//
            let record : SimpleRecord option = None
            // Check the data serializes as expected
            let expectedJson = """null"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

        testCase "should create a simple anonymous record schema" <| fun _ ->
            // Title isn't great - But the rest of the model is good. Title
            // won't be like this in practice
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "\u003C\u003Ef__AnonymousType3179181557\u00603",
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
}"""

            let record = {| A = "Hello"; B = 42; C = false |}
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"A":"Hello","B":42,"C":false}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))


        testCase "should create a simple record schema with some options and nullables" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "SimpleRecordWithSomeNullablesAndOptions",
  "type": "object",
  "properties": {
    "Ao": {
      "type": "string"
    },
    "Bo": {
      "type": "integer",
      "format": "int32"
    },
    "Co": {
      "type": "integer",
      "format": "int64"
    },
    "D": {
      "type": "boolean"
    }
  },
  "required": [
    "D"
  ]
}"""

            let record = { Ao = Some "Hello"; Bo = Nullable(); Co = None; D = true }
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"Ao":"Hello","D":true}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))


        testCase "should create a empty object schema for a private record" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "properties": {}
}"""

            let record = createPrivateRecord "Hello" 852
            let schema = draft4FromType (record.GetType()) |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))


        testCase "should create a record schema with a single record" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "SingleReferencedRecord",
  "type": "object",
  "properties": {
    "Simple": {
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
    },
    "Value": {
      "type": "number",
      "format": "decimal"
    }
  },
  "required": [
    "Simple",
    "Value"
  ]
}"""

            let record = { Simple = { A = "Hello"; B = 42; C = false }
                           Value = 22.22m }
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"Simple":{"A":"Hello","B":42,"C":false},"Value":22.22}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))


        testCase "should create a record schema with multiple layers of records" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "MultiLayerNestingRecord",
  "type": "object",
  "properties": {
    "Single": {
      "type": "object",
      "properties": {
        "Simple": {
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
        },
        "Value": {
          "type": "number",
          "format": "decimal"
        }
      },
      "required": [
        "Simple",
        "Value"
      ]
    }
  },
  "required": [
    "Single"
  ]
}"""

            let record = { Single = { Simple = { A = "Hello"; B = 42; C = false }
                                      Value = 22.22m }}
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"Single":{"Simple":{"A":"Hello","B":42,"C":false},"Value":22.22}}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let schema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(schema))


        testCase "should create a record schema referencing optional records" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "OptionalRecordReferencesRecord",
  "type": "object",
  "properties": {
    "Xo": {
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
    },
    "Yo": {
      "type": "object",
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
  }
}"""

            let dt = DateTime(2024,09,16,16,48,18).ToUniversalTime()
            let record = { Xo = Some { A = "Hello"; B = 42; C = false }
                           Yo = Some { Alpha = dt; Beta = 98.211163m } }
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            //** With all values present **//
            // Check the data serializes as expected
            let expectedJson = """{"Xo":{"A":"Hello","B":42,"C":false},"Yo":{"Alpha":"2024-09-16T15:48:18Z","Beta":98.211163}}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

            //** With some values present **//
            // Check the data serializes as expected
            let noneValue = { Xo = None
                              Yo = Some { Alpha = dt; Beta = 98.211163m } }
            let expectedJson = """{"Yo":{"Alpha":"2024-09-16T15:48:18Z","Beta":98.211163}}"""
            let json = FSharpLuLikeSerializer.serialize noneValue
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

            //** With no values present **//
            // Check the data serializes as expected
            let noneValue = { Xo = None
                              Yo = None }
            let expectedJson = """{}"""
            let json = FSharpLuLikeSerializer.serialize noneValue
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))


        testCase "should create a record schema referencing multiple layers of optional records" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "MultiLayerNestingOptionalRecord",
  "type": "object",
  "properties": {
    "Ro": {
      "type": "object",
      "properties": {
        "Xo": {
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
        },
        "Yo": {
          "type": "object",
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
      }
    }
  }
}"""

            let record =
                { Ro = Some { Xo = Some { A = "Hello"; B = 42; C = false }
                              Yo = None } }
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            //** With all values present **//
            // Check the data serializes as expected
            let expectedJson = """{"Ro":{"Xo":{"A":"Hello","B":42,"C":false}}}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

            //** With no values present **//
            // Check the data serializes as expected
            let noneValue = { Ro = None }
            let expectedJson = """{}"""
            let json = FSharpLuLikeSerializer.serialize noneValue
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))


        testCase "should create a record schema with multiple references to the same type" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "MultipleReferencedRecord",
  "type": "object",
  "properties": {
    "X": {
      "$ref": "#/definitions/SimpleRecord"
    },
    "Y": {
      "$ref": "#/definitions/SimpleRecord"
    }
  },
  "required": [
    "X",
    "Y"
  ],
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
            let record =
                { X = { A = "Hello"; B = 12; C = true }
                  Y = { A = "World"; B = -1234; C = false } }
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"X":{"A":"Hello","B":12,"C":true},"Y":{"A":"World","B":-1234,"C":false}}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))


        testCase "should create a record schema with multiple optional references to the same type" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "MultipleReferencedOptionalRecord",
  "type": "object",
  "properties": {
    "X1o": {
      "$ref": "#/definitions/SimpleRecord"
    },
    "X2o": {
      "$ref": "#/definitions/SimpleRecord"
    }
  },
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
            let record =
                { X1o = Some { A = "Hello"; B = 12; C = true }
                  X2o = Some { A = "World"; B = -1234; C = false } }
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            //** With all values present **//
            // Check the data serializes as expected
            let expectedJson = """{"X1o":{"A":"Hello","B":12,"C":true},"X2o":{"A":"World","B":-1234,"C":false}}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

            //** With no values present **//
            // Check the data serializes as expected
            let noneValue = { X1o = None; X2o = None }
            let expectedJson = """{}"""
            let json = FSharpLuLikeSerializer.serialize noneValue
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

    // TODO: Record model with nested Anonymous Records
    // TODO: Type names that mess with json schema naming, especially for references: i.e. / or $
    // TODO: Multiple different records with the same Record name
    // let record =
    //     { M1Record1 = { Value = "Hello" }
    //       M1Record2 = { Value = "There" }
    //       M2Record1 = { Value = "Crazy" }
    //       M2Record2 = { Value = "World" } }

    // TODO: Generic Types on the Record
    // TODO: Generic Types on the Records properties
    // TODO: Generic Types on the Records properties recursive?

]