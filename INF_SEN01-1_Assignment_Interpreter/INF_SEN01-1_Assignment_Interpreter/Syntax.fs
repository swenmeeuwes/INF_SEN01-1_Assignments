module Syntax

type Types =
    | Int of int
    | Float of float
    | Double of double
    | String of string
    | Boolean of bool

type Expression =
    | Sum of Expression * Expression
    | Subtract of Expression * Expression
    | Divide of Expression * Expression
    | Multiply of Expression * Expression