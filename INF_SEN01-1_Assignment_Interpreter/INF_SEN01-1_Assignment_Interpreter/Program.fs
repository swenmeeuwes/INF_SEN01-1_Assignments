open Syntax
open Interpreter

let someNumber = Float 3.0
let anotherNumber = Integer 2

//let commands = Add (Subtract (someNumber, anotherNumber), Subtract (someNumber, anotherNumber))
let commands = Subtract(someNumber, anotherNumber)

let result = 
    try 
        eval commands
    with
    | Exception(str) -> StringType(str)


[<EntryPoint>]
let main argv = 
    printfn "%A" result
    0 // return an integer exit code
