module Syntax

type ValueType =
    | IntegerType of int
    | FloatType of float
    | DoubleType of double
    | StringType of string
    | BooleanType of bool

type Expression =
    | Value of ValueType
    | Variable of string

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
    | Block of Statement list
    | If of Expression * Statement
    | IfElse of Expression * Statement * Statement
    | While of Expression * Statement
    | Declaration of string * Expression
    | Printfn of Expression

// Module record to act as a wrapper, which contains variables and statements
type KobraModule = {
    variables: Map<string, ValueType>
    statements: Statement list
}

let Integer x = Value(IntegerType x);
let Float x = Value(FloatType x);
let Double x = Value(DoubleType x);
let String x = Value(StringType x);
let Boolean x = Value(BooleanType x);