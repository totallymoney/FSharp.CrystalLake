module Model

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open FSharp.Reflection.FSharpReflectionExtensions

module Array =

    let foldi<'T, 'State>
        (folder: int -> 'State -> 'T -> 'State)
        (state : 'State)
        (array : 'T [])
        : 'State
        =
        ((0, state), array)
        ||> Array.fold (fun (i,state) item ->
            let nextState = folder i state item
            i + 1, nextState)
        |> snd

    let sequenceResultM (arr : Result<'ok,'err> []) : Result<'ok [],'err> =
        let folder i (acc : Result<'ok [],'err>) s =
            match acc with
            | Error _ -> acc
            | Ok acc ->
                match s with
                | Error err -> Error err
                | Ok s -> acc.[i] <- s
                          Ok acc

        let resultArray : 'ok [] = Array.zeroCreate<'ok> arr.Length
        (Ok resultArray, arr) ||> foldi folder

let isOption (typ: Type) =
    typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<_ option>

let isNullable (typ : Type) =
    Nullable.GetUnderlyingType typ <> null

type ValueType =
    // .net reference types
    | Guid
    | String
    | Decimal
    | DateTime
    | DateTimeOffset
    | DateOnly
    | TimeOnly
    // .net primitive types
    | Boolean
    | Char
    | Int8
    | UInt8
    | Int16
    | UInt16
    | Int32
    | UInt32
    | Int64
    | UInt64
    | Float
    | Double

type ValueModel =
    { Title : string
      UnderlyingType : Type
      ValueType : ValueType
      // Keep nullable and optional separate in case we want to handle them
      // differently in the future
      Nullable : bool
      Optional : bool }

and PropertyRepresentation =
    | Inline of ModelRepresentation
    | Reference of string
    | SelfRecursive // as opposed to mutually recursive
    | RootNodeReference // root node is a special case

and PropertyModel =
    { Name : string
      UnderlyingProperty : PropertyInfo
      // ResolvedType is normally `UnderlyingProperty.PropertyType`, but with
      // optionals it may get unwrapped
      ResolvedType : Type
      Optional : bool
      Representation : PropertyRepresentation }

and RecordModel =
    { Title : string option
      UnderlyingType : Type
      Properties : PropertyModel []
      Optional : bool }
      // Keep nullable and optional separate in case we want to handle them
      // differently in the future
      // NOTE: Record types are never optional or nullable - it's the properties
      // they are on.
      // Nullable : bool
      // Optional : bool }

and TupleElement =
    | InlineElement of ModelRepresentation
    | ReferenceElement of Type * string
    | OptionalElement of TupleElement

and TupleModel =
    { Title : string option
      UnderlyingType : Type
      Optional : bool
      TupleElements : TupleElement [] }

// TODO: Think up a better name
and ModelRepresentation =
    | Value of ValueModel
    | Record of RecordModel
    | Tuple of TupleModel
    // I'm not sure if I like having a special case for this, though in many
    // ways required vs not required is a special case in JsonSchema.
    // I think once I do DUs I can figure out if it's actually a special case
    // or not
    | Optional of ModelRepresentation
    // In some situations we want to leave types as unevaluated and come back to
    // them later
    | Unevaluated of Type

module ModelRepresentation =
    let rec getUnderlyingType = function
        | Unevaluated x -> x
        | Value x -> x.UnderlyingType
        | Record x -> x.UnderlyingType
        | Tuple x -> x.UnderlyingType
        | Optional x -> getUnderlyingType x

    let rec getTitle = function
        | Unevaluated x -> x.Name // TODO - This probably won't cut it for long - we'll need a name resolver
        | Value x -> x.Title
        | Record x -> defaultArg x.Title "Record" // TODO - a bit of placehoder, but I think it will be ok
        | Tuple x -> defaultArg x.Title "Tuple" // TODO - a bit of placehoder, but I think it will be ok
        | Optional x -> getTitle x

module TupleElement =
    //let rec getUnderlyingType = function
    //    | ReferenceElement (typ, str) -> typ
    //    | InlineElement x -> ModelRepresentation.getUnderlyingType x
    //    | OptionalElement x -> getUnderlyingType x

    let rec getTitle = function
        | ReferenceElement (typ, str) -> str
        | InlineElement x -> ModelRepresentation.getTitle x
        | OptionalElement x -> getTitle x



let defaultValueModel =
    { Title = ""
      UnderlyingType = typeof<System.String>
      ValueType = String
      Nullable = false
      Optional = false }


type ModelResult =
    { Model : ModelRepresentation
      // Note: `Unevaluated` can and will have duplicates.
      // i.e. when the same record appears in multiple properties
      Unevaluated : Type [] }

type SkippableEvaluationResult =
    | ModelEvaluation of ModelResult
    | ErrorEvaluation of string
    | NoMatch

module SkippableEvaluationResult =
    // It's basically Option.orElseWith, but considers the two parts of an
    // EvaluationResult which could be None
    let orElseWith ifNoneThunk evaluationResult =
        match evaluationResult with
        | ModelEvaluation _ -> evaluationResult
        | ErrorEvaluation _ -> evaluationResult
        | NoMatch -> ifNoneThunk ()

    let map mapping evaluationResult =
        match evaluationResult with
        | ModelEvaluation model -> mapping model
        | ErrorEvaluation _ -> evaluationResult
        | NoMatch -> evaluationResult

type EvaluationResult =
    | ModelResult of ModelResult
    | EvalError of string

module EvaluationResult =
    let map mapping evaluationResult =
        match evaluationResult with
        | ModelResult model -> ModelResult (mapping model)
        | EvalError _ -> evaluationResult

    let toResult evaluationResult =
        match evaluationResult with
        | ModelResult model -> Ok model
        | EvalError err -> Error err

type TypeEvaluation =
    { RootModel : ModelRepresentation
      Definitions : Map<string, ModelRepresentation> }

let determineValueType (typ: Type) : ValueType option =
    if typ = typeof<String> then Some String
    elif typ = typeof<System.Char> then Some Char
    elif typ = typeof<System.Guid> then Some Guid
    elif typ = typeof<decimal> then Some Decimal
    elif typ = typeof<DateTime> then Some DateTime
    elif typ = typeof<DateTimeOffset> then Some DateTimeOffset
    elif typ = typeof<DateOnly> then Some DateOnly
    elif typ = typeof<TimeOnly> then Some TimeOnly
    elif typ = typeof<Boolean> then Some Boolean
    elif typ = typeof<int8> then Some Int8
    elif typ = typeof<uint8> then Some UInt8
    elif typ = typeof<int16> then Some Int16
    elif typ = typeof<uint16> then Some UInt16
    elif typ = typeof<int32> then Some Int32
    elif typ = typeof<uint32> then Some UInt32
    elif typ = typeof<int64> then Some Int64
    elif typ = typeof<uint64> then Some UInt64
    elif typ = typeof<float32> then Some Float
    elif typ = typeof<float> then Some Double
    else None

let isValueType (typ: Type) : bool =
    determineValueType typ <> None

let tryEvaluateValueType (typ: Type) : SkippableEvaluationResult =
    match determineValueType typ with
    | None -> NoMatch
    | Some vt ->
        let valueModel =
            Value { defaultValueModel with
                      Title = typ.Name
                      UnderlyingType = typ
                      ValueType = vt }
        ModelEvaluation { Model = valueModel; Unevaluated = [||] }

let rec evaluateProperty root
                         (prop: PropertyInfo)
                         : Result<PropertyModel * Type [], string> =
    match tryEvaluateShallow root prop.PropertyType with
    | EvalError err -> Error err
    | ModelResult modelResult ->
        match modelResult.Model with
        | Value _ ->
            ({ Name = prop.Name
               UnderlyingProperty = prop
               ResolvedType = prop.PropertyType
               Optional = false
               Representation = Inline modelResult.Model },
             [||]) |> Ok
        | Optional (Value _ as v) ->
            ({ Name = prop.Name
               UnderlyingProperty = prop
               ResolvedType = v.GetType()
               Optional = true
               Representation = Inline v },
             [||]) |> Ok
        | Unevaluated typ when typ = root ->
            ({ Name = prop.Name
               UnderlyingProperty = prop
               ResolvedType = typ
               Optional = false
               Representation = RootNodeReference },
             [||]) |> Ok
        | Unevaluated typ when typ.Name = prop.ReflectedType.Name ->
            ({ Name = prop.Name
               UnderlyingProperty = prop
               ResolvedType = typ
               Optional = false
               Representation = SelfRecursive },
             [||]) |> Ok
        | Unevaluated typ ->
            ({ Name = prop.Name
               UnderlyingProperty = prop
               ResolvedType = typ
               Optional = false
               Representation = Reference typ.Name },
             [| typ |]) |> Ok
        | Optional (Unevaluated typ) when typ = root ->
            ({ Name = prop.Name
               UnderlyingProperty = prop
               ResolvedType = typ
               Optional = true
               Representation = RootNodeReference },
             [||]) |> Ok
        | Optional (Unevaluated typ) when typ.Name = prop.ReflectedType.Name  ->
            ({ Name = prop.Name
               UnderlyingProperty = prop
               ResolvedType = typ
               Optional = true
               Representation = SelfRecursive },
             [||]) |> Ok
        | Optional (Unevaluated typ) ->
            ({ Name = prop.Name
               UnderlyingProperty = prop
               ResolvedType = typ
               Optional = true
               Representation = Reference typ.Name },
             [| typ |]) |> Ok
        | Tuple m ->
            ({ Name = prop.Name
               UnderlyingProperty = prop
               ResolvedType = m.UnderlyingType
               Optional = m.Optional
               Representation = Inline (Tuple m) },
             modelResult.Unevaluated) |> Ok
        | Optional (Tuple m) ->
            ({ Name = prop.Name
               UnderlyingProperty = prop
               ResolvedType = m.UnderlyingType
               Optional = true
               Representation = Inline (Tuple m) },
             modelResult.Unevaluated) |> Ok
        | Optional (Optional _) -> failwithf "Should not reach here. Optionals should be unrolled"
        | Optional (Record _)
        | Record _ -> failwithf "Should not reach here. We performed a shallow evaluation"

and tryEvaluateRecordType root (typ: Type) : SkippableEvaluationResult =
    if (FSharpType.IsRecord typ) then
        let fields = FSharpType.GetRecordFields typ
        let propertyEvaluationResult =
            fields
            |> Array.map (evaluateProperty root)
            |> Array.sequenceResultM

        match propertyEvaluationResult with
        | Error err -> ErrorEvaluation err
        | Ok result ->
            let properties, unevaluated = result |> Array.unzip

            let recordModel =
                { Title = Some typ.Name
                  UnderlyingType = typ
                  Properties = properties
                  Optional = false }

            ModelEvaluation { Model = Record recordModel
                              Unevaluated = Array.concat unevaluated }

    elif (FSharpType.IsRecord (typ, true)) then
        // I don't think this is the best way to handle private record, but I
        // wanted to keep it compatible with `System.Text.Json` for now.
        // I think it would be better to error here since it's probably a mistake
        // if someone is trying to serialize private data.
        let recordModel =
            { Title = None
              UnderlyingType = typ
              Properties = [||]
              Optional = false }

        ModelEvaluation { Model = Record recordModel
                          Unevaluated = [||] }
    else
        NoMatch

and tryEvaluateTupleType root (typ: Type) : SkippableEvaluationResult =
    if FSharpType.IsTuple typ
    then
        let tupleElements = FSharpType.GetTupleElements typ

        let tupleEvaluationResult =
            tupleElements
            |> Array.map (fun elementType ->
                tryEvaluateShallow root elementType |> EvaluationResult.toResult)
            |> Array.sequenceResultM

        match tupleEvaluationResult with
        | Error err -> ErrorEvaluation err
        | Ok tupleElements ->
            let unevaluated = tupleElements |> Array.map _.Unevaluated |> Array.concat

            let tupleElements =
                tupleElements
                |> Array.map _.Model
                |> Array.map (fun m ->
                    match m with
                    | Unevaluated typ -> ReferenceElement (typ, typ.Name)
                    | Optional (Unevaluated typ) -> OptionalElement (ReferenceElement (typ, typ.Name))
                    | m -> InlineElement m )

            let elementTitles = tupleElements |> Array.map TupleElement.getTitle

            let model =
                { TupleModel.Title = Some ("TupleOf" + String.Join("And", elementTitles))
                  UnderlyingType = typ
                  Optional = false
                  TupleElements = tupleElements }

            ModelEvaluation { Model = Tuple model
                              Unevaluated = unevaluated }
    else
        NoMatch

and tryEvaluateOptionType evaluator root (typ: Type) : SkippableEvaluationResult =
    if isOption typ then
        let cases = FSharpType.GetUnionCases(typ)
        let someCase = cases |> Array.find (fun case -> case.Name = "Some")
        let someType = someCase.GetFields().[0].PropertyType
        match evaluator root someType with
        | EvalError err -> ErrorEvaluation err
        | ModelResult modelResult ->
            let model =
                match modelResult.Model with
                | Unevaluated _ -> Optional modelResult.Model // keep it as unevaluated, we just wrap it in an optional
                | Optional o -> o // we flatten optional types, not sure if this is right atm. TODO: Needs a test case
                | Value model -> Value { model with Optional = true }
                | Tuple model -> Tuple { model with Optional = true }
                | Record model ->  Record { model with Optional = true }
            ModelEvaluation { modelResult with Model = model }
    else
        NoMatch

and tryEvaluateNullableType evaluator root (typ: Type) : SkippableEvaluationResult =
    let underlyingType = Nullable.GetUnderlyingType typ
    if underlyingType <> null then
        match evaluator root underlyingType with
        | EvalError err -> ErrorEvaluation err
        | ModelResult modelResult ->
            let model =
                match modelResult.Model with
                | Value model -> Value { model with Nullable =  true }
                | Record _ // Records aren't nullable in .Net atm
                | Tuple _ // Tuples aren't nullable in .Net atm... I think?
                | Optional _ -> // DUs aren't nullable in .Net atm
                    failwithf "Should not reach here. Only values are nullable "
                | Unevaluated _ ->
                    failwithf "Should not reach here. Only values are nullable, and we always evaluate them"
            ModelEvaluation { modelResult with Model = model }
    else
        NoMatch

// Same as tryEvaluate, but it won't evaluate records
and tryEvaluateShallow root (typ: Type) : EvaluationResult =
    tryEvaluateValueType typ
    |> SkippableEvaluationResult.orElseWith (fun () ->
        tryEvaluateTupleType root typ)
    |> SkippableEvaluationResult.orElseWith (fun () ->
        tryEvaluateOptionType tryEvaluateShallow root typ)
    |> SkippableEvaluationResult.orElseWith (fun () ->
        tryEvaluateNullableType tryEvaluateShallow root typ)
    |> function
        | ModelEvaluation x -> ModelResult x
        | ErrorEvaluation err -> EvalError err
        | NoMatch -> ModelResult { Model = Unevaluated typ; Unevaluated = [| typ |] }

and tryEvaluate root (typ: Type) : EvaluationResult =
    tryEvaluateValueType typ
    |> SkippableEvaluationResult.orElseWith (fun () ->
        tryEvaluateRecordType root typ)
    |> SkippableEvaluationResult.orElseWith (fun () ->
        tryEvaluateTupleType root typ)
    |> SkippableEvaluationResult.orElseWith (fun () ->
        tryEvaluateOptionType tryEvaluate root typ)
    |> SkippableEvaluationResult.orElseWith (fun () ->
        tryEvaluateNullableType tryEvaluate root typ)
    |> function
        | ModelEvaluation x -> ModelResult x
        | ErrorEvaluation err -> EvalError err
        | NoMatch -> EvalError $"Type: {typ.FullName} is not currently supported"

and replaceWithInlineRecord (replaceAt: ModelRepresentation) (replaceWith: ModelRepresentation) =
    let replaceWithType = ModelRepresentation.getUnderlyingType replaceWith
    match replaceAt with
    | Value x ->
        if x.UnderlyingType = replaceWithType
        then failwith "Value types are not replaceable"
        else replaceAt
    | Unevaluated _ -> failwith "Should not reach here. Unevaluated should be replaced while building the models"
    | Optional m -> Optional (replaceWithInlineRecord m replaceWith)
    | Record r ->
        let updatedProperties =
            r.Properties |> Array.map (fun p ->
                if p.ResolvedType = replaceWithType
                then { p with Representation = Inline replaceWith }
                else p)
        Record { r with Properties = updatedProperties }
    | Tuple m ->
        let updatedElements =
            m.TupleElements |> Array.map (fun e ->
                match e with
                | InlineElement e ->
                    InlineElement (replaceWithInlineRecord e replaceWith)
                | OptionalElement (InlineElement e) ->
                    OptionalElement (InlineElement (replaceWithInlineRecord e replaceWith))
                | ReferenceElement (typ, _) ->
                    if typ = replaceWithType
                    then InlineElement replaceWith
                    else e
                | OptionalElement (ReferenceElement (typ, _)) ->
                    if typ = replaceWithType
                    then OptionalElement (InlineElement replaceWith)
                    else e
                | OptionalElement (OptionalElement _) ->
                    failwith "Nested Optional Elements aren't supported here")
        Tuple { m with TupleElements = updatedElements }

// At this point every records properties pointing to other records are marked
// as Unevaluated. We just have a flat list of models that reference each other.
// Where it makes sense, we'd like to replace Unevaluated properties with InPlace
// objects to turn this into a tree of records.
// Where a record type is used in multiple places we'll keep it as a separate
// definition to avoid duplication or circular references, and change Unevaluated
// models to a Reference
and convertToTree
    (definitions : Map<string, ModelRepresentation>)
    (evaluatedList : string list)
    (referenceUsages : Map<string, string list>) =
    // Evaluated list is reversed. So going through it in order will replace the
    // correct items for non-mutable trees

    let mutable definitions = definitions

    // We will need to handle the root node differently than other nodes because
    // we need to avoid moving it to definitions, or removing it completely
    let rootNode = List.last evaluatedList

    evaluatedList |> List.iter (fun typeName ->
        // The root node should never be replaced or moved inline to another node
        if typeName = rootNode then () else
        let replaceWith = Map.find typeName definitions
        // If we have multiple (or zero) instances of this type we want to leave
        // it as a definition to be referenced. Otherwise if we have a single
        // instance put that model inplace.
        match Map.tryFind typeName referenceUsages with
        | None -> ()
        | Some [ singleUsage ] ->
            let replaceAt = Map.find singleUsage definitions
            let replacedDefinition = replaceWithInlineRecord replaceAt replaceWith
            definitions <- Map.add singleUsage replacedDefinition definitions
            // remove the definition item since we've embedded it inside another definition
            definitions <- Map.remove typeName definitions
        | Some _ -> () )

    definitions

and runEvaluation (rootType: Type) : Result<TypeEvaluation, string> =
    let mutable evaluatedList : string list = []
    let mutable evaluated : Map<string, ModelRepresentation> = Map.empty
    let mutable unevaluated : Type list = [ rootType ]
    // Remember each place where a record is used
    let mutable referenceUsages : Map<string, string list> = Map.empty
    let mutable error: string option = None

    while error = None && not (List.isEmpty unevaluated) do
        let next = unevaluated.Head
        unevaluated <- unevaluated.Tail

        let result = tryEvaluate rootType next
        match result with
        | EvalError err -> error <- Some err
        | ModelResult result ->
            // TODO: Should probably use the full name, but we'll figure that out later
            evaluated <- Map.add next.Name result.Model evaluated
            evaluatedList <- next.Name::evaluatedList

            // Note: result.Unevaluated can and will have duplicates. i.e. when
            // the same record appears in multiple properties

            // For each unevaluated item, remember where we found it
            result.Unevaluated
            |> Array.iter (fun typ ->
                match Map.tryFind typ.Name referenceUsages with
                | None ->
                    referenceUsages <- Map.add typ.Name [next.Name] referenceUsages
                | Some locations ->
                    referenceUsages <- Map.add typ.Name (next.Name::locations) referenceUsages)

            // Add items we haven't evaluated to the end of the list of items to evaluate
            unevaluated <-
                result.Unevaluated
                |> Array.filter (fun t -> not (Map.containsKey t.Name evaluated))
                |> List.ofArray
                |> List.append unevaluated
                |> List.distinct

    match error with
    | Some error -> Error error
    | None ->
        let evaluated = convertToTree evaluated evaluatedList referenceUsages

        Ok { RootModel = Map.find rootType.Name evaluated
             Definitions = Map.remove rootType.Name evaluated }


and evaluateType (typ: Type) : Result<TypeEvaluation, string> =
    runEvaluation typ

let evaluateObj<'t> (_: 't) =
    // doing _.GetType() can do weird unboxing stuff
    // e.g. `Nullable<int32>(42).GetType()` returns `System.Int32` which is
    // not what we want. Doing it this way will give us the correct underlying
    // type we want to use
    runEvaluation typeof<'t>
