module Interpreter

open System
open Syntax

exception Exception of string

// Helpers
let floatToInt subject = int(System.Math.Round(subject: float))

// Evaluate expression
let rec evaluateExpression variables expression =
    match expression with
    | Value v -> v
    | Variable key ->
        match Map.tryFind key variables with
        | Some value -> value
        | None -> raise (Exception ("Variable '" + key + "' was never declared."))
    
    // Binary operators
    | Multiply (left, right) -> 
        (fun left right -> 
            match left, right with
            | IntegerType n1, IntegerType n2    -> IntegerType (n1 * n2)
            | FloatType n1, FloatType n2        -> FloatType (n1 * n2)
            | DoubleType n1, DoubleType n2      -> DoubleType (n1 * n2)                    
            | _                                 -> raise (Exception "Cannot multiply, incompatible types.")
        ) (evaluateExpression variables left) (evaluateExpression variables right)
    | Divide (left, right) -> 
        (fun left right -> 
            match left, right with
            | IntegerType n1, IntegerType n2    -> IntegerType (n1 / n2)
            | FloatType n1, FloatType n2        -> FloatType (n1 / n2)
            | DoubleType n1, DoubleType n2      -> DoubleType (n1 / n2)                    
            | _                                 -> raise (Exception "Cannot divide, incompatible types.")
        ) (evaluateExpression variables left) (evaluateExpression variables right)
    | Add (left, right) -> 
        (fun left right -> 
            match left, right with
            | IntegerType n1, IntegerType n2    -> IntegerType (n1 + n2)
            | FloatType n1, FloatType n2        -> FloatType (n1 + n2)
            | DoubleType n1, DoubleType n2      -> DoubleType (n1 + n2)
            | StringType n1, StringType n2      -> StringType (n1 + n2) // Concatenation
            | _                                 -> raise (Exception "Cannot sum, incompatible types.")
        ) (evaluateExpression variables left) (evaluateExpression variables right)
    | Subtract (left, right) -> 
        (fun left right -> 
            match left, right with
            | IntegerType n1, IntegerType n2    -> IntegerType (n1 - n2)
            | FloatType n1, FloatType n2        -> FloatType (n1 - n2)
            | DoubleType n1, DoubleType n2      -> DoubleType (n1 - n2)
            | _                                 -> raise (Exception "Cannot subtract, incompatible types.")
        ) (evaluateExpression variables left) (evaluateExpression variables right)
    | Modulo (left, right) -> 
        (fun left right -> 
            match left, right with
            | IntegerType n1, IntegerType n2    -> IntegerType (n1 % n2)
            | _                                 -> raise (Exception "Cannot execute modulo operation, incompatible types.")
        ) (evaluateExpression variables left) (evaluateExpression variables right)
    
    // Logical operators
    | GreaterThan (left, right) ->
        (fun left right ->
            match left, right with
            | IntegerType n1, IntegerType n2    -> BooleanType (n1 > n2)
            | FloatType n1, FloatType n2        -> BooleanType (n1 > n2)
            | DoubleType n1, DoubleType n2      -> BooleanType (n1 > n2)
            | _                                 -> raise (Exception "Cannot execute logical check 'GreaterThan', incompatible types.")
        ) (evaluateExpression variables left) (evaluateExpression variables right)
    | LessThan (left, right) ->
        (fun left right ->
            match left, right with
            | IntegerType n1, IntegerType n2    -> BooleanType (n1 < n2)
            | FloatType n1, FloatType n2        -> BooleanType (n1 < n2)
            | DoubleType n1, DoubleType n2      -> BooleanType (n1 < n2)
            | _                                 -> raise (Exception "Cannot execute logical check 'LessThan', incompatible types.")
        ) (evaluateExpression variables left) (evaluateExpression variables right)
    | Equals (left, right) ->
        (fun left right ->
            match left, right with
            | IntegerType n1, IntegerType n2    -> BooleanType (n1 = n2)
            | FloatType n1, FloatType n2        -> BooleanType (n1 = n2)
            | DoubleType n1, DoubleType n2      -> BooleanType (n1 = n2)
            | BooleanType n1, BooleanType n2    -> BooleanType (n1 = n2)
            | _                                 -> raise (Exception "Cannot execute logical check 'Equals', incompatible types.")
        ) (evaluateExpression variables left) (evaluateExpression variables right)
    | Or (left, right) ->
        (fun left right ->
            match left, right with
            | BooleanType n1, BooleanType n2    -> BooleanType (n1 || n2)
            | _                                 -> raise (Exception "Cannot execute logical check 'Or', incompatible types.")
        ) (evaluateExpression variables left) (evaluateExpression variables right)
    | And (left, right) ->
        (fun left right ->
            match left, right with
            | BooleanType n1, BooleanType n2    -> BooleanType (n1 && n2)
            | _                                 -> raise (Exception "Cannot execute logical check 'And', incompatible types.")
        ) (evaluateExpression variables left) (evaluateExpression variables right)
    | Not x ->
        (fun x ->
            match x with
            | BooleanType n1                    -> BooleanType (not n1)
            | _                                 -> raise (Exception "Cannot execute logical check 'Not', incompatible type.")
        ) (evaluateExpression variables x)

// Evaluate statement
let rec evaluateStatement variables statement : Map<string, ValueType> =
    match statement with
    | Block (statementList) -> 
        statementList |>
            List.fold (fun updatedVariables statement -> evaluateStatement updatedVariables statement) variables
    | If (expression, statement) ->
        (fun expression ->
            match expression with
            | BooleanType n1                    -> if(n1 = true) then (evaluateStatement variables statement) else variables
            | _                                 -> raise (Exception "Given expression is not of type Boolean.")
        ) (evaluateExpression variables expression)
    | IfElse (expression, statementIf, statementElse) ->
        (fun expression ->
            match expression with
            | BooleanType n1                    -> if(n1 = true) then (evaluateStatement variables statementIf) else (evaluateStatement variables statementElse)
            | _                                 -> raise (Exception "Given expression is not of type Boolean.")
        ) (evaluateExpression variables expression)
    | Declaration (variableName, expression) ->
            variables |>
            Map.add variableName (evaluateExpression variables expression)
    | Printfn expression ->
        (fun expression ->
            do printfn "%A" expression
            variables
        ) (evaluateExpression variables expression)
    | While (expression, block) ->
        (fun expression ->
            match expression with
            | BooleanType n1                    -> if(n1 = true) then (
                                                        let newVariables = evaluateStatement variables block 
                                                        evaluateStatement newVariables statement) 
                                                   else variables
            | _                                 -> raise (Exception "Given Expression is not of type Boolean.") 
        )(evaluateExpression variables expression)

// Evaluate module
let evaluate (kobraModule: KobraModule) =
    kobraModule.statements |>
    List.fold (fun updatedVariables statement -> evaluateStatement updatedVariables statement) kobraModule.variables