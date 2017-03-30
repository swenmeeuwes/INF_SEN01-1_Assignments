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