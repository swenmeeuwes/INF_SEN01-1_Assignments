open Syntax
open Interpreter

let someNumber = Integer 2
let anotherNumber = Integer 3

//let result = eval (Add (Value(someNumber), Value(anotherNumber)))
let result = eval (Add (Add (someNumber, anotherNumber), Add (someNumber, anotherNumber)))
// Wish: Add someNumber anotherNumber

[<EntryPoint>]
let main argv = 
    printfn "%A" result
    0 // return an integer exit code
