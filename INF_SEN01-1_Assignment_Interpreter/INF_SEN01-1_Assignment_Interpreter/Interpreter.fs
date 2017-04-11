module Interpreter

open System
open Syntax

exception Exception of string

// Helpers
let floatToInt subject = int(System.Math.Round(subject: float))

// evaluate expression
let rec evaluateExpression expression =
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
        ) (evaluateExpression left) (evaluateExpression right)
    | Divide (left, right) -> 
        (fun left right -> 
            match left, right with
            | IntegerType n1, IntegerType n2    -> IntegerType (n1 / n2)
            | FloatType n1, FloatType n2        -> FloatType (n1 / n2)
            | DoubleType n1, DoubleType n2      -> DoubleType (n1 / n2)                    
            | _                                 -> raise (Exception "Cannot divide, incompatible types.")
        ) (evaluateExpression left) (evaluateExpression right)
    | Add (left, right) -> 
        (fun left right -> 
            match left, right with
            | IntegerType n1, IntegerType n2    -> IntegerType (n1 + n2)
            | FloatType n1, FloatType n2        -> FloatType (n1 + n2)
            | DoubleType n1, DoubleType n2      -> DoubleType (n1 + n2)
            | StringType n1, StringType n2      -> StringType (n1 + n2) // Concatenation
            | _                                 -> raise (Exception "Cannot sum, incompatible types.")
        ) (evaluateExpression left) (evaluateExpression right)
    | Subtract (left, right) -> 
        (fun left right -> 
            match left, right with
            | IntegerType n1, IntegerType n2    -> IntegerType (n1 - n2)
            | FloatType n1, FloatType n2        -> FloatType (n1 - n2)
            | DoubleType n1, DoubleType n2      -> DoubleType (n1 - n2)
            | _                                 -> raise (Exception "Cannot subtract, incompatible types.")
        ) (evaluateExpression left) (evaluateExpression right)
    | Modulo (left, right) -> 
        (fun left right -> 
            match left, right with
            | IntegerType n1, IntegerType n2    -> IntegerType (n1 % n2)
            | _                                 -> raise (Exception "Cannot execute modulo operation, incompatible types.")
        ) (evaluateExpression left) (evaluateExpression right)
    
    // Logical operators
    | GreaterThan (left, right) ->
        (fun left right ->
            match left, right with
            | IntegerType n1, IntegerType n2    -> BooleanType (n1 > n2)
            | FloatType n1, FloatType n2        -> BooleanType (n1 > n2)
            | DoubleType n1, DoubleType n2      -> BooleanType (n1 > n2)
            | _                                 -> raise (Exception "Cannot execute logical check 'GreaterThan', incompatible types.")
        ) (evaluateExpression left) (evaluateExpression right)
    | LessThan (left, right) ->
        (fun left right ->
            match left, right with
            | IntegerType n1, IntegerType n2    -> BooleanType (n1 < n2)
            | FloatType n1, FloatType n2        -> BooleanType (n1 < n2)
            | DoubleType n1, DoubleType n2      -> BooleanType (n1 < n2)
            | _                                 -> raise (Exception "Cannot execute logical check 'LessThan', incompatible types.")
        ) (evaluateExpression left) (evaluateExpression right)
    | Equals (left, right) ->
        (fun left right ->
            match left, right with
            | IntegerType n1, IntegerType n2    -> BooleanType (n1 = n2)
            | FloatType n1, FloatType n2        -> BooleanType (n1 = n2)
            | DoubleType n1, DoubleType n2      -> BooleanType (n1 = n2)
            | _                                 -> raise (Exception "Cannot execute logical check 'Equals', incompatible types.")
        ) (evaluateExpression left) (evaluateExpression right)
    | Or (left, right) ->
        (fun left right ->
            match left, right with
            | BooleanType n1, BooleanType n2    -> BooleanType (n1 || n2)
            | _                                 -> raise (Exception "Cannot execute logical check 'Or', incompatible types.")
        ) (evaluateExpression left) (evaluateExpression right)
    | And (left, right) ->
        (fun left right ->
            match left, right with
            | BooleanType n1, BooleanType n2    -> BooleanType (n1 && n2)
            | _                                 -> raise (Exception "Cannot execute logical check 'And', incompatible types.")
        ) (evaluateExpression left) (evaluateExpression right)
    | Not x ->
        (fun x ->
            match x with
            | BooleanType n1                    -> BooleanType (not n1)
            | _                                 -> raise (Exception "Cannot execute logical check 'Not', incompatible type.")
        ) (evaluateExpression x)

let rec evaluateStatement statement =
    match statement with
    | Printfn expression ->
        (fun expression ->
            printfn "%A" expression
        ) (evaluateExpression expression)
    | If (expression, statement) ->
        (fun expression ->
            match expression with
            | BooleanType n1                    -> if(n1 = true) then (evaluateStatement statement)
            | _                                 -> raise (Exception "Given expression is not of type Boolean.")
        ) (evaluateExpression expression)
    | IfElse (expression, statementIf, statementElse) ->
        (fun expression ->
            match expression with
            | BooleanType n1                    -> if(n1 = true) then (evaluateStatement statementIf) else (evaluateStatement statementElse)
            | _                                 -> raise (Exception "Given expression is not of type Boolean.")
        ) (evaluateExpression expression)
            
