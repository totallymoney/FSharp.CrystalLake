module TestTypes

open System

let draft4FromObj obj =
    match Model.evaluateObj obj with
    | Ok evalResult -> Draft4.fromModel evalResult
    | Error errorStr -> failwith errorStr

let draft4FromType typ =
    match Model.evaluateType typ with
    | Ok evalResult -> Draft4.fromModel evalResult
    | Error errorStr -> failwith errorStr


type SimpleRecord = { A : string; B : int32; C : bool }

type SimpleRecord2 = { Alpha : DateTime; Beta : decimal }

type SingleReferencedRecord = { Simple : SimpleRecord; Value : decimal }

type MultiLayerNestingRecord = { Single : SingleReferencedRecord }

type MultipleReferencedRecord = { X : SimpleRecord; Y : SimpleRecord }

type MultipleReferencedOptionalRecord = { X1o : SimpleRecord option; X2o : SimpleRecord option }

type ConsRecord =
    { Value : int32
      Next : ConsRecord option }

type TreeNodeRecord =
    { Value : int32
      Left : TreeNodeRecord option
      Right : TreeNodeRecord option }

type ReferencedConsRecord =
    { Cons: ConsRecord }

type MultipleConsRecords =
    { ConsA : ConsRecord
      ConsB : ConsRecord }

type SimpleRecordWithSomeNullablesAndOptions =
    { Ao : string option
      Bo : Nullable<int32>
      Co : int64 option
      D : bool }

type OptionalRecordReferencesRecord =
    { Xo : SimpleRecord option
      Yo : SimpleRecord2 option }

type MultiLayerNestingOptionalRecord =
    { Ro : OptionalRecordReferencesRecord option }


type private PrivateRecord = { Ap : string; Bp : int32;}

let createPrivateRecord a b : Object =
    { Ap = a; Bp = b }

type SingleCaseDu = SingleCase of int

type SingleCaseOptionDu = SingleCaseOption of int option

type SingleCaseDuRecord = SingleCaseRecord of SimpleRecord

module Module1 =
    type InnerRecord = { Value : string }

module Module2 =
    type InnerRecord = { Value : string }

type OuterRecord =
    { M1Record1 : Module1.InnerRecord
      M1Record2 : Module1.InnerRecord
      M2Record1 : Module2.InnerRecord
      M2Record2 : Module2.InnerRecord }

type MutualRecordInt =
    { Value : int32
      SubString : MutualRecordString option }

and MutualRecordString =
    { Value : string
      SubInt : MutualRecordInt option }

type MutualTreeNodeA =
    { Value : string
      LeftB : MutualTreeNodeB option
      RightB : MutualTreeNodeB option }

and MutualTreeNodeB =
    { Value : bool
      LeftA : MutualTreeNodeA option
      RightA : MutualTreeNodeA option }

type MutualTreeNodeC =
    { Value : string
      LeftC : MutualTreeNodeC option
      RightD : MutualTreeNodeD option }

and MutualTreeNodeD =
    { Value : bool
      LeftD : MutualTreeNodeD option
      RightC : MutualTreeNodeC option }

type GenericTuple<'a> = char * 'a

type MultiGenericTuple<'a, 'b> = 'a * 'b

type GenericOptTuple<'a> = Guid * 'a option

type RecordWithTuple =
    { Value : int
      Tuple : string * decimal }


type RecordWithOptionalTuple =
    { Value : int
      OptTuple : (string * decimal) option }

type RecordWithMultipleTuples =
    { Value : int
      TupleA : char * byte
      TupleB : string * decimal}

type RecordWithMultipleIdenticalTuples =
    { Value : int
      Tuple1a : string * decimal
      Tuple1b : string * decimal}

type RecursiveRecordWithTuple =
    { Value : int
      RecursiveTuple : string * RecursiveRecordWithTuple option }

type RecursiveRecordOnNestedTuple =
    { Value : int
      RecursiveNestedTuple : string * (double * RecursiveRecordWithTuple option) }

type MultiplyRecursiveRecordWithTuple =
    { Value : int
      RecursiveTupleA : (RecursiveRecordWithTuple * int64) option
      RecursiveTupleB : string * (RecursiveRecordWithTuple option * double) }

type SimpleIntEnum =
    | FirstCase = 1
    | SecondCase = 2
    | ThirdCase = 3

type SimpleDUEnum =
    | FirstOption
    | SecondOption
    | ThirdOption

type DUWithPrimitives =
    | StringCase of string
    | IntCase of int64
    | BoolCase of bool option

type DUWithRecords =
    | RecordA of SimpleRecord
    | RecordB of SimpleRecord2

type SingleCaseDUWithTuple =
    | SingleCaseTuple of int * string

type DUWithTuples =
    | TupleA of int * string
    | TupleB of SimpleRecord * bool

type DUWith2Types =
    | DUItemA
    | DUItemB of SimpleRecord2

// (no fields, single field, multiple multiple fields)
type DUWithMixedTypes =
    | MixedA
    | MixedB of SimpleRecord2
    | MixedC of int * bool * string
