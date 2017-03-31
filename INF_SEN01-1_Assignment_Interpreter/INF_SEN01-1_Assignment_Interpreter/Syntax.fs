module Syntax

type ValueType =
    | IntegerType of int
    | FloatType of float
    | DoubleType of double
    | StringType of string
    | BooleanType of bool

type Expression =
    | Value of ValueType
    | Add of Expression * Expression
    | Subtract of Expression * Expression
    | Divide of Expression * Expression
    | Multiply of Expression * Expression

let Integer x = Value(IntegerType x);
let Float x = Value(FloatType x);
let Double x = Value(DoubleType x);
let String x = Value(StringType x);
let Boolean x = Value(BooleanType x);