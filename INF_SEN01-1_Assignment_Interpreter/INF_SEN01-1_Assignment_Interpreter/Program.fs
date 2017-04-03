open Syntax
open Interpreter

let someNumber = Float 3.0
let anotherNumber = Float 2.0

//let commands = Add (Subtract (someNumber, anotherNumber), Subtract (someNumber, anotherNumber))
//let commands = GreaterThan(someNumber, anotherNumber)
let commands = Not (Equals(Add(Float 3.0, Float 3.0), Float 6.0))

let result = 
    try 
        eval commands
    with
    | Exception(str) -> StringType(str)


[<EntryPoint>]
let main argv = 
    printfn "%A" result
    0 // return an integer exit code
