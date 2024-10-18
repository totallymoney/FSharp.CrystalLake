module RecursiveRecordTests

open Newtonsoft.Json.Linq
open Newtonsoft.Json.Schema
open Expecto
open Expecto.Flip
open TestTypes

[<Tests>]
let tests =
    testList "FSharp recursive record schema generation tests" [
        testCase "should create a schema for a non-root recursive record with single usage" <| fun _ ->
            // NOTE: We could have made "Cons" a definitions and had that refer
            // to itself, but this works equally well
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "ReferencedConsRecord",
  "type": "object",
  "properties": {
    "Cons": {
      "type": "object",
      "properties": {
        "Next": {
          "$ref": "#/properties/Cons"
        },
        "Value": {
          "type": "integer",
          "format": "int32"
        }
      },
      "required": [
        "Value"
      ]
    }
  },
  "required": [
    "Cons"
  ]
}"""

            let record =
                { Cons = { Value = 555
                           Next = Some { Value = 244
                                         Next = None } } }
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"Cons":{"Value":555,"Next":{"Value":244}}}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))


        testCase "should create a schema for a non-root recursive record with multiple usages as a definition" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "MultipleConsRecords",
  "type": "object",
  "properties": {
    "ConsA": {
      "$ref": "#/definitions/ConsRecord"
    },
    "ConsB": {
      "$ref": "#/definitions/ConsRecord"
    }
  },
  "required": [
    "ConsA",
    "ConsB"
  ],
  "definitions": {
    "ConsRecord": {
      "type": "object",
      "properties": {
        "Next": {
          "$ref": "#/definitions/ConsRecord"
        },
        "Value": {
          "type": "integer",
          "format": "int32"
        }
      },
      "required": [
        "Value"
      ]
    }
  }
}"""
            let record =
               { ConsA = { Value = 555
                           Next = Some { Value = 244
                                         Next = None } }
                 ConsB = { Value = -999
                           Next = None } }
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"ConsA":{"Value":555,"Next":{"Value":244}},"ConsB":{"Value":-999}}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))


        testCase "should create a Recursive Root Record" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "ConsRecord",
  "type": "object",
  "properties": {
    "Next": {
      "$ref": "#"
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

            let record =
                { Value = 555
                  Next = Some { Value = 244
                                Next = Some { Value = 22
                                              Next = None } } }

            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"Value":555,"Next":{"Value":244,"Next":{"Value":22}}}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))


        testCase "should create a schema for a recursive root record with multiple recursive references" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "TreeNodeRecord",
  "type": "object",
  "properties": {
    "Left": {
      "$ref": "#"
    },
    "Right": {
      "$ref": "#"
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

            let record =
                { Value = 555
                  Left = Some { Value = 244
                                Left = None
                                Right = Some { Value = 22
                                               Left = None
                                               Right = None } }
                  Right = Some { Value = -888
                                 Left = Some { Value = 0
                                               Left = None
                                               Right = None }
                                 Right = None }}

            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"Value":555,"Left":{"Value":244,"Right":{"Value":22}},"Right":{"Value":-888,"Left":{"Value":0}}}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))


        testCase "should create a schema for a non-root mutually recursive record" <| fun _ ->
            // It might look nicer if MutualRecordInt was part of the main
            // properties and not a definition, but this is a valid schema.
            // I'd have to add some type of loop detection if we wanted to do
            // this in a nicer way.
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "\u003C\u003Ef__AnonymousType603971316\u00601",
  "type": "object",
  "properties": {
    "Data": {
      "$ref": "#/definitions/MutualRecordInt"
    }
  },
  "required": [
    "Data"
  ],
  "definitions": {
    "MutualRecordInt": {
      "type": "object",
      "properties": {
        "SubString": {
          "type": "object",
          "properties": {
            "SubInt": {
              "$ref": "#/definitions/MutualRecordInt"
            },
            "Value": {
              "type": "string"
            }
          },
          "required": [
            "Value"
          ]
        },
        "Value": {
          "type": "integer",
          "format": "int32"
        }
      },
      "required": [
        "Value"
      ]
    }
  }
}"""
            let record =
                {| Data =
                    { Value = 555
                      SubString =
                          Some { Value = "Hello"
                                 SubInt =
                                     Some { Value = 22
                                            SubString = None } } } |}
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"Data":{"Value":555,"SubString":{"Value":"Hello","SubInt":{"Value":22}}}}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

        testCase "should create a schema for a mutually recursive record at root" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "MutualRecordInt",
  "type": "object",
  "properties": {
    "SubString": {
      "type": "object",
      "properties": {
        "SubInt": {
          "$ref": "#"
        },
        "Value": {
          "type": "string"
        }
      },
      "required": [
        "Value"
      ]
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
            let record =
              { Value = 555
                SubString =
                  Some { Value = "Hello"
                         SubInt =
                           Some { Value = 22
                                  SubString = None } } }
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"Value":555,"SubString":{"Value":"Hello","SubInt":{"Value":22}}}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))


        testCase "should create a schema for a non-root mutually recursive tree" <| fun _ ->
            // It might look nicer if MutualTreeNodeA was part of the main
            // properties and not a definition, but this is a valid schema.
            // I'd have to add some type of loop detection if we wanted to do
            // this in a nicer way.
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "\u003C\u003Ef__AnonymousType603971316\u00601",
  "type": "object",
  "properties": {
    "Data": {
      "$ref": "#/definitions/MutualTreeNodeA"
    }
  },
  "required": [
    "Data"
  ],
  "definitions": {
    "MutualTreeNodeA": {
      "type": "object",
      "properties": {
        "LeftB": {
          "$ref": "#/definitions/MutualTreeNodeB"
        },
        "RightB": {
          "$ref": "#/definitions/MutualTreeNodeB"
        },
        "Value": {
          "type": "string"
        }
      },
      "required": [
        "Value"
      ]
    },
    "MutualTreeNodeB": {
      "type": "object",
      "properties": {
        "LeftA": {
          "$ref": "#/definitions/MutualTreeNodeA"
        },
        "RightA": {
          "$ref": "#/definitions/MutualTreeNodeA"
        },
        "Value": {
          "type": "boolean"
        }
      },
      "required": [
        "Value"
      ]
    }
  }
}"""
            let record =
                {| Data =
                    { Value = "Root"
                      LeftB = Some { Value = false
                                     LeftA = None
                                     RightA = Some { Value = "Left-Right"
                                                     LeftB = None
                                                     RightB = None } }
                      RightB = Some { Value = true
                                      LeftA = Some { Value = "Right-Left"
                                                     LeftB = Some { Value = false
                                                                    LeftA = None
                                                                    RightA = None }
                                                     RightB = None }
                                      RightA = None }} |}
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"Data":{"Value":"Root","LeftB":{"Value":false,"RightA":{"Value":"Left-Right"}},"RightB":{"Value":true,"LeftA":{"Value":"Right-Left","LeftB":{"Value":false}}}}}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

        testCase "should create a schema for a root based mutually recursive tree" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "MutualTreeNodeA",
  "type": "object",
  "properties": {
    "LeftB": {
      "$ref": "#/definitions/MutualTreeNodeB"
    },
    "RightB": {
      "$ref": "#/definitions/MutualTreeNodeB"
    },
    "Value": {
      "type": "string"
    }
  },
  "required": [
    "Value"
  ],
  "definitions": {
    "MutualTreeNodeB": {
      "type": "object",
      "properties": {
        "LeftA": {
          "$ref": "#"
        },
        "RightA": {
          "$ref": "#"
        },
        "Value": {
          "type": "boolean"
        }
      },
      "required": [
        "Value"
      ]
    }
  }
}"""
            let record =
                { Value = "Root"
                  LeftB = Some { Value = false
                                 LeftA = None
                                 RightA = Some { Value = "Left-Right"
                                                 LeftB = None
                                                 RightB = None } }
                  RightB = Some { Value = true
                                  LeftA = Some { Value = "Right-Left"
                                                 LeftB = Some { Value = false
                                                                LeftA = None
                                                                RightA = None }
                                                 RightB = None }
                                  RightA = None }}
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"Value":"Root","LeftB":{"Value":false,"RightA":{"Value":"Left-Right"}},"RightB":{"Value":true,"LeftA":{"Value":"Right-Left","LeftB":{"Value":false}}}}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))


        testCase "should create a schema for a root based mutually recursive tree alternating" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "MutualTreeNodeC",
  "type": "object",
  "properties": {
    "LeftC": {
      "$ref": "#"
    },
    "RightD": {
      "type": "object",
      "properties": {
        "LeftD": {
          "$ref": "#/properties/RightD"
        },
        "RightC": {
          "$ref": "#"
        },
        "Value": {
          "type": "boolean"
        }
      },
      "required": [
        "Value"
      ]
    },
    "Value": {
      "type": "string"
    }
  },
  "required": [
    "Value"
  ]
}"""
            let record =
                { Value = "Root"
                  LeftC = Some { Value = "Left"
                                 LeftC = None
                                 RightD = Some { Value = true
                                                 LeftD = None
                                                 RightC = None } }
                  RightD = Some { Value = false
                                  LeftD = Some { Value = true
                                                 LeftD = Some { Value = false
                                                                LeftD = None
                                                                RightC = None }
                                                 RightC = None }
                                  RightC = None }}
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"Value":"Root","LeftC":{"Value":"Left","RightD":{"Value":true}},"RightD":{"Value":false,"LeftD":{"Value":true,"LeftD":{"Value":false}}}}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))


        testCase "should create a schema for a non-root mutually recursive tree alternating" <| fun _ ->
            let expectedSchema = """{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "title": "\u003C\u003Ef__AnonymousType603971316\u00601",
  "type": "object",
  "properties": {
    "Data": {
      "$ref": "#/definitions/MutualTreeNodeC"
    }
  },
  "required": [
    "Data"
  ],
  "definitions": {
    "MutualTreeNodeC": {
      "type": "object",
      "properties": {
        "LeftC": {
          "$ref": "#/definitions/MutualTreeNodeC"
        },
        "RightD": {
          "type": "object",
          "properties": {
            "LeftD": {
              "$ref": "#/definitions/MutualTreeNodeC/properties/RightD"
            },
            "RightC": {
              "$ref": "#/definitions/MutualTreeNodeC"
            },
            "Value": {
              "type": "boolean"
            }
          },
          "required": [
            "Value"
          ]
        },
        "Value": {
          "type": "string"
        }
      },
      "required": [
        "Value"
      ]
    }
  }
}"""
            let record =
                {| Data =
                    { Value = "Root"
                      LeftC = Some { Value = "Left"
                                     LeftC = None
                                     RightD = Some { Value = true
                                                     LeftD = None
                                                     RightC = None } }
                      RightD = Some { Value = false
                                      LeftD = Some { Value = true
                                                     LeftD = Some { Value = false
                                                                    LeftD = None
                                                                    RightC = None }
                                                     RightC = None }
                                      RightC = None }} |}
            let schema = draft4FromObj record |> Helper.toJson
            Expect.equal "" expectedSchema schema

            // Check the data serializes as expected
            let expectedJson = """{"Data":{"Value":"Root","LeftC":{"Value":"Left","RightD":{"Value":true}},"RightD":{"Value":false,"LeftD":{"Value":true,"LeftD":{"Value":false}}}}}"""
            let json = FSharpLuLikeSerializer.serialize record
            Expect.equal "" expectedJson json

            // Check the serialized json matches the schema
            let jSchema = JSchema.Parse(schema)
            let jToken = JToken.Parse(json)
            Expect.isTrue "Json is not valid for schema" (jToken.IsValid(jSchema))

]