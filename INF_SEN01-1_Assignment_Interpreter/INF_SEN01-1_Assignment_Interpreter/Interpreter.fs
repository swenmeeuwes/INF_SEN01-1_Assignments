module Interpreter

open System
open Syntax

exception Exception of string

// Helpers
let floatToInt subject = int(System.Math.Round(subject: float))

// Evaluation
let rec eval expression =
    match expression with
    | Value v -> v
    | Multiply (left, right) -> 
        (fun left right -> 
            match left, right with
            | IntegerType n1, IntegerType n2    -> IntegerType (n1 * n2)
            | FloatType n1, FloatType n2        -> FloatType (n1 * n2)
            | DoubleType n1, DoubleType n2      -> DoubleType (n1 * n2)                    
            | _                                 -> raise (Exception "Cannot multiply, incompatible types.")
        ) (eval left) (eval right)
    | Divide (left, right) -> 
        (fun left right -> 
            match left, right with
            | IntegerType n1, IntegerType n2    -> IntegerType (n1 / n2)
            | FloatType n1, FloatType n2        -> FloatType (n1 / n2)
            | DoubleType n1, DoubleType n2      -> DoubleType (n1 / n2)                    
            | _                                 -> raise (Exception "Cannot divide, incompatible types.")
        ) (eval left) (eval right)
    | Add (left, right) -> 
        (fun left right -> 
            match left, right with
            | IntegerType n1, IntegerType n2    -> IntegerType (n1 + n2)
            | FloatType n1, FloatType n2        -> FloatType (n1 + n2)
            | DoubleType n1, DoubleType n2      -> DoubleType (n1 + n2)
            | StringType n1, StringType n2      -> StringType (n1 + n2) // Concatenation
            | _                                 -> raise (Exception "Cannot sum, incompatible types.")
        ) (eval left) (eval right)
    | Subtract (left, right) -> 
        (fun left right -> 
            match left, right with
            | IntegerType n1, IntegerType n2    -> IntegerType (n1 - n2)
            | FloatType n1, FloatType n2        -> FloatType (n1 - n2)
            | DoubleType n1, DoubleType n2      -> DoubleType (n1 - n2)
            | _                                 -> raise (Exception "Cannot subtract, incompatible types.")
        ) (eval left) (eval right)

            // Add default case -> raise exception
            // Not needed since all Expression cases are handled