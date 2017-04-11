module Syntax

//let TrueType = fun t f -> t
//let FalseType = fun t f -> f

//type BooleanType =
//    | TrueType
//    | FalseType

type ValueType =
    | IntegerType of int
    | FloatType of float
    | DoubleType of double
    | StringType of string
    | BooleanType of bool

type Expression =
    | Value of ValueType

    // Binary operators
    | Add of Expression * Expression
    | Subtract of Expression * Expression
    | Divide of Expression * Expression
    | Multiply of Expression * Expression
    | Modulo of Expression * Expression

    // Logical operators
    | GreaterThan of Expression * Expression
    | LessThan of Expression * Expression
    | Equals of Expression * Expression
    | And of Expression * Expression
    | Or of Expression * Expression
    | Not of Expression

type Statement =
    | Printfn of Expression
    | If of Expression * Statement
    | IfElse of Expression * Statement * Statement
    | Block of Statement list
    | While of Expression * Statement

let Integer x = Value(IntegerType x);
let Float x = Value(FloatType x);
let Double x = Value(DoubleType x);
let String x = Value(StringType x);
let Boolean x = Value(BooleanType x);