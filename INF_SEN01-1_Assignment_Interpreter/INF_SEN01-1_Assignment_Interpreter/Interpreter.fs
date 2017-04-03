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
    
    // Binary operators
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
    | Modulo (left, right) -> 
        (fun left right -> 
            match left, right with
            | IntegerType n1, IntegerType n2    -> IntegerType (n1 % n2)
            | _                                 -> raise (Exception "Cannot execute modulo operation, incompatible types.")
        ) (eval left) (eval right)
    
    // Logical operators
    | GreaterThan (left, right) ->
        (fun left right ->
            match left, right with
            | IntegerType n1, IntegerType n2    -> BooleanType (n1 > n2)
            | FloatType n1, FloatType n2        -> BooleanType (n1 > n2)
            | DoubleType n1, DoubleType n2      -> BooleanType (n1 > n2)
            | _                                 -> raise (Exception "Cannot execute logical check 'GreaterThan', incompatible types.")
        ) (eval left) (eval right)
    | LessThan (left, right) ->
        (fun left right ->
            match left, right with
            | IntegerType n1, IntegerType n2    -> BooleanType (n1 < n2)
            | FloatType n1, FloatType n2        -> BooleanType (n1 < n2)
            | DoubleType n1, DoubleType n2      -> BooleanType (n1 < n2)
            | _                                 -> raise (Exception "Cannot execute logical check 'LessThan', incompatible types.")
        ) (eval left) (eval right)
    | Equals (left, right) ->
        (fun left right ->
            match left, right with
            | IntegerType n1, IntegerType n2    -> BooleanType (n1 = n2)
            | FloatType n1, FloatType n2        -> BooleanType (n1 = n2)
            | DoubleType n1, DoubleType n2      -> BooleanType (n1 = n2)
            | _                                 -> raise (Exception "Cannot execute logical check 'Equals', incompatible types.")
        ) (eval left) (eval right)
    | Or (left, right) ->
        (fun left right ->
            match left, right with
            | BooleanType n1, BooleanType n2    -> BooleanType (n1 || n2)
            | _                                 -> raise (Exception "Cannot execute logical check 'Or', incompatible types.")
        ) (eval left) (eval right)
    | And (left, right) ->
        (fun left right ->
            match left, right with
            | BooleanType n1, BooleanType n2    -> BooleanType (n1 && n2)
            | _                                 -> raise (Exception "Cannot execute logical check 'And', incompatible types.")
        ) (eval left) (eval right)
    | Not (x) ->
        (fun x ->
            match x with
            | BooleanType n1                    -> BooleanType (not n1)
            | _                                 -> raise (Exception "Cannot execute logical check 'Not', incompatible type.")
        ) (eval x)
        // Default case not needed since all Expression cases should be handled