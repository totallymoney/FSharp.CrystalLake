module Draft4

open System
open Model

type TypeField =
    | Simple of string
    | Composite of string []

// Json Schema draft 4 RFC:
// https://datatracker.ietf.org/doc/html/draft-zyp-json-schema-03#anchor8

and Draft4SchemaNode =
    { title : string option
      ``type`` : TypeField option
      format : string option
      items : Draft4SchemaNode [] option
      oneOf: Draft4SchemaNode [] option
      properties : Map<string, Draft4SchemaNode> option
      required : string [] option
      additionalItems : bool option
      additionalProperties : bool option
      // In draft versions 4-7 all other properties are ignored if $ref is set
      ``$ref``: string option }


type Draft4Schema =
    { ``$schema`` : string
      title : string option
      // type is technically optional per the spec for v4, but we will always
      // have it at the root schema
      ``type`` : TypeField
      format : string option
      items : Draft4SchemaNode [] option
      oneOf: Draft4SchemaNode [] option
      properties : Map<string, Draft4SchemaNode> option
      required : string [] option
      additionalItems : bool option
      additionalProperties : bool option
      definitions : Map<string, Draft4SchemaNode> option }

module Draft4Schema =
    let fromSchemaNode (node : Draft4SchemaNode) =
        { ``$schema`` = "http://json-schema.org/draft-04/schema#"
          title = node.title
          ``type`` = node.``type`` |> Option.get
          format = node.format
          items = node.items
          oneOf = node.oneOf
          properties = node.properties
          required = node.required
          additionalItems = node.additionalItems
          additionalProperties = node.additionalProperties
          definitions = None }


let defaultDraft4SchemaNode =
    { title = None
      ``type`` = None
      format = None
      items = None
      oneOf = None
      properties = None
      required = None
      additionalItems = None
      additionalProperties = None
      ``$ref`` = None }

let nullSchemaNode =
    { defaultDraft4SchemaNode with ``type`` = Some (Simple "null") }

type SchemaPathNode =
    { PathKey: string
      CrystalLakeRef: string option }

module SchemaPath =
    let pathToString nodes =
        let schemaPath = nodes |> List.map _.PathKey
        System.String.Join("/", schemaPath)

    let rec prevRefPath ref nodes =
        match nodes with
        | [] -> None
        | x::xs when x.CrystalLakeRef = Some ref -> Some nodes
        | _::xs -> prevRefPath ref xs

    let simple pathKey =
        { PathKey = pathKey; CrystalLakeRef = None }

// draft 4, etc supports 7 primitive types:
//   array    A JSON array.
//   boolean  A JSON boolean.
//   integer  A JSON number without a fraction or exponent part.
//   number   Any JSON number.  Number includes integer.
//   null     The JSON null value.
//   object   A JSON object.
//   string   A JSON string.
// (it gets rid of the "any" type from draft 3)
// it supports composite types as well ["string", "null"]

let typeFromValueType (typ : ValueType) =
    match typ with
    | Guid -> "string"
    | String -> "string"
    | Char -> "string"
    | Boolean -> "boolean"
    | Decimal | Float | Double -> "number"
    | Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 | Int64 | UInt64 -> "integer"
    | DateTime | DateTimeOffset | DateOnly | TimeOnly -> "string"


let typeFromValue (m : ValueModel) =
    let typeString = typeFromValueType m.ValueType

    if (m.Optional || m.Nullable)
    then Composite [| typeString; "null" |]
    else Simple typeString

// Optional/nullable properties work a bit different than stand-alone values.
// They are optional/nullable by default and should use the "Required" field on
// the object to describe properties that can't be null. While having a
// composite type: [ "integer", "null" ] would still work, it's redundant and
// not really in spirit of the format
let rec typeFromPropertyModel (m : ModelRepresentation) =
    match m with
    | Optional m -> typeFromPropertyModel m
    | Value m -> Simple (typeFromValueType m.ValueType)
    | Record m -> Simple "object"
    | Tuple m -> Simple "array"
    | Unevaluated _ -> failwith "Draft should not have unevaluated models"

// draft 3 and followups
// https://datatracker.ietf.org/doc/html/draft-zyp-json-schema-03#anchor27
// support some specific formats, but also allow us to define our own

let formatFromValueType (typ : ValueType) =
    match typ with
    | String -> None
    | Char -> Some "char"
    | Boolean -> None
    | DateTime -> Some "date-time" // SHOULD be a date in ISO 8601 format of YYYY-MM-DDThh:mm:ssZ in UTC time
    | DateTimeOffset -> Some "date-time" // not quite the standard type because of the +XX, but it's only a "SHOULD"
    | DateOnly -> Some "date" // SHOULD be a date in the format of YYYY-MM-DD
    | TimeOnly -> Some "time" // SHOULD be a time in the format of hh:mm:ss
    | Guid -> Some "uuid" // not in the draft-4 standard, but in future drafts and ok here
    // these int types are not part of any standard, but supported by some systems
    | Int8 -> Some "int8" | UInt8 -> Some "uint8"
    | Int16 -> Some "int16" | UInt16 -> Some "uint16"
    | Int32 -> Some "int32" | UInt32 -> Some "uint32"
    | Int64 -> Some "int64" | UInt64 -> Some "uint64"
    | Double -> Some "double" | Float -> Some "float"
    | Decimal -> Some "decimal"

let rec buildProperty (schemaPath : SchemaPathNode list)
                      (defs : Map<string, ModelRepresentation>)
                      (prop : PropertyModel) =
    match prop.Representation with
    | Inline representation ->
        let propPath = SchemaPath.simple prop.Name
        prop.Name,
        { buildNode representation (propPath::schemaPath) defs with
            // while having a title would be valid in a technical sense,
            // it's redundant and potentially confusing since it's in the
            // property name, so we clear it
            title = None
            // be particular with type to handle options and nullables
            ``type`` = Some (typeFromPropertyModel representation) }
    | RootNodeReference ->
        prop.Name, { defaultDraft4SchemaNode with ``$ref`` = Some "#" }
    | Reference str ->
        prop.Name,
        { defaultDraft4SchemaNode with ``$ref`` = Some $"#/definitions/{str}" }
    | SelfRecursive ->
        // TODO: We'll have to be smarter here when we deal with arrays, and DUs
        let schemaPath =
            schemaPath
            |> List.tail // head will be "properties"
            |> List.rev // schemaPath is in reverse order
        let jsonPointer = SchemaPath.pathToString schemaPath
        prop.Name, { defaultDraft4SchemaNode with ``$ref`` = Some jsonPointer }


and propertiesFromRecord (m : RecordModel)
                         (schemaPath : SchemaPathNode list)
                         (defs : Map<string, ModelRepresentation>) =
    let schemaPath = { PathKey = "properties"; CrystalLakeRef = None }:: schemaPath
    m.Properties
    |> Array.map (buildProperty schemaPath defs)
    // TODO: converting to a map losses the ordering we have. The properties on
    // RecordModel are in the order they are defined, and switching it to a map
    // loses that information. The resulting schema is still valid, just not
    // quite as nice
    |> Map

and requiredPropertiesFromRecord (m : RecordModel) =
    m.Properties
    |> Array.filter (fun p ->
        if p.Optional then false
        else
            match p.Representation with
            | Inline (Optional _) -> false
            | Inline (Value m) -> not m.Optional && not m.Nullable
            | _ -> true)
    |> Array.map _.Name
    |> function
        | [||] -> None
        | xs -> Some xs

and itemsFromTuple (tupleElements : TupleElement array)
                   (schemaPath : SchemaPathNode list)
                   (defs : Map<string, ModelRepresentation>)
                   : Draft4SchemaNode [] =
    let schemaPath = SchemaPath.simple "items" :: schemaPath
    tupleElements
    |> Array.mapi (fun i e ->
        let elementPath = SchemaPath.simple (string i) :: schemaPath
        match e with
        | InlineElement m ->
            match m with
            | Value _  ->
                // It doesn't add anything to include a title for a value type
                // in a tuple
                { buildNode m elementPath defs with title = None }
            | _ -> buildNode m elementPath defs
        | OptionalElement (InlineElement m) ->
            buildNode (Optional m) elementPath defs
        | ReferenceElement (typ, str) ->
            let path =
                SchemaPath.prevRefPath str schemaPath
                |> Option.map SchemaPath.pathToString
                |> Option.orElse (Some $"#/definitions/{str}")
            { defaultDraft4SchemaNode with ``$ref`` = path }
        | OptionalElement (ReferenceElement (typ, str)) ->
            let path =
                SchemaPath.prevRefPath str schemaPath
                |> Option.map SchemaPath.pathToString
                |> Option.orElse (Some $"#/definitions/{str}")
            // normally with optionals we can just add a "null" to the type, but
            // that's not valid with $refs that aren't supposed to have any
            // other types. So we have to use the more long-winded `oneOf`
            let ref = { defaultDraft4SchemaNode with ``$ref`` = path }
            { defaultDraft4SchemaNode with
               oneOf = Some [| ref; nullSchemaNode |] }
        | OptionalElement (OptionalElement _)  ->
            failwith "Nested optional elements are not currently supported")

and buildNode (m : ModelRepresentation)
              (schemaPath : SchemaPathNode list)
              (defs : Map<string, ModelRepresentation>)
              : Draft4SchemaNode =
    match m with
    | Value m ->
        { defaultDraft4SchemaNode with
            title = Some m.Title
            ``type`` = Some (typeFromValue m)
            format = formatFromValueType m.ValueType }
    | Record m ->
        let schemaType =
            if m.Optional
            then Composite [|"object"; "null"|]
            else Simple "object"
        { defaultDraft4SchemaNode with
            title = m.Title
            ``type`` = Some schemaType
            properties = Some (propertiesFromRecord m schemaPath defs)
            required = requiredPropertiesFromRecord m }
    | Tuple m ->
        let schemaType =
            if m.Optional
            then Composite [|"array"; "null"|]
            else Simple "array"
        { defaultDraft4SchemaNode with
            title = m.Title
            ``type`` = Some schemaType
            items = Some (itemsFromTuple m.TupleElements schemaPath defs)
            // A tuple has a fixed size, we don't support additional items, and
            // it can't have properties like an object
            additionalItems = Some false
            additionalProperties =  Some false }
    | Optional m ->
        let node = buildNode m schemaPath defs
        let newType =
            match node.``type`` with
            // if it doesn't have a type how is it optional... fail
            | None -> failwith "Cannot have an optional of something without a type"
            // an optional of something that can only be null, is just null
            | Some (Simple "null") -> Some (Simple "null")
            // and if it's a single type add null to make it optional
            | Some (Simple x) -> Some (Composite [| x; "null"|])
            // otherwise if it's composite append null
            | Some (Composite xs) ->
                let newComposite = Array.append xs [| "null"|] |> Array.distinct
                Some (Composite newComposite)
        { node with ``type`` = newType }
    | Unevaluated _ -> failwithf "buildNode Unevaluated is not supported"

let buildDefinitions (schemaPath : SchemaPathNode list) (eval : TypeEvaluation) =
    let schemaPath = { PathKey = "definitions"; CrystalLakeRef = None }::schemaPath
    // We'll need a better way to resolve names and deal with conflicts
    eval.Definitions.Values
    |> Seq.map (fun model ->
        let defName = (ModelRepresentation.getUnderlyingType model).Name
        let pathNode = { PathKey = defName; CrystalLakeRef = Some defName }
        defName,
        { buildNode model (pathNode::schemaPath) eval.Definitions with
            // strip out the title from the definition since it will
            // be part of the property name
            title = None })
    |> Map

let fromModel (eval : TypeEvaluation) =
    // TODO: We need to figure out the names before doing anything else
    // should we do that in the Model code though??
    let schemaPath =
        { PathKey = "#"
          CrystalLakeRef = Some (ModelRepresentation.getTitle eval.RootModel) }
    let schema =
        Draft4Schema.fromSchemaNode
            (buildNode eval.RootModel [schemaPath] eval.Definitions)
    let definitions = buildDefinitions [schemaPath] eval
    match Map.isEmpty definitions with
    | false -> { schema with definitions = Some definitions }
    | true -> schema


