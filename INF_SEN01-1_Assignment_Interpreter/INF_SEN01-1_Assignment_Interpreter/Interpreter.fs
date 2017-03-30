module Interpreter

open Syntax

exception Exception of string

let rec eval expression =
    match expression with
            | Value v -> v
            | Add (left, right) -> 
                (fun left right -> 
                    match left, right with
                    | IntegerType n1, IntegerType n2    -> IntegerType (n1 + n2)
                    | FloatType n1, FloatType n2        -> FloatType (n1 + n2)
                    | DoubleType n1, DoubleType n2      -> DoubleType (n1 + n2)
                    | StringType n1, StringType n2      -> StringType (n1 + n2)
                    | _                         -> raise (Exception "Cannot sum, incompatible types.")
                ) (eval left) (eval right)