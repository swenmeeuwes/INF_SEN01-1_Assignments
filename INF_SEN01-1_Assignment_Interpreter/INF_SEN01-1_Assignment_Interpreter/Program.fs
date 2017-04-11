open ModuleLoader
open Syntax
open Interpreter
open Parser

let someNumber = Float 3.0
let anotherNumber = Float 2.0

//let commands = Add (Subtract (someNumber, anotherNumber), Subtract (someNumber, anotherNumber))
//let commands = GreaterThan(someNumber, anotherNumber)
//let commands = Not (Equals(Add(Float 3.0, Float 3.0), Float 6.0))
//let commands = If(Equals(GreaterThan(Integer 4, Integer 3), Equals(Integer 3, Integer 3)), Printfn(Integer 4))
let commands = Block([Printfn(Integer 3); Printfn(Integer 45)])
let result = 
    try 
        evaluateStatement commands
    with
    | Exception(str) -> printfn "%A" (str)

//let result = 
//    try 
//        evaluateExpression commands
//    with
//    | Exception(str) -> StringType(str)

// TO BE CONTINUED
//let tokens =
//    interpretModule "SampleProgram.kobra" [|' '|]

//let result =
//    Seq.map parseToken tokens

[<EntryPoint>]
let main argv = 
    //printfn "%A" result

    0 // return an integer exit code
