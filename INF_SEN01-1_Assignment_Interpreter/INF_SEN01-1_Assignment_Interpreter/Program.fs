open ModuleLoader
open Syntax
open Interpreter
open Parser

// Sample Kobra (the "Python like language") modules
let moduleHelloWorld = {
    variables = Map.empty
    statements = [
        Printfn (String "Hello world!")
    ]
}

let moduleConditionalHelloWorld = {
    variables = Map.empty
    statements = [
        IfElse(Equals(Float 3.2, Float 8.1), Printfn (String "Hello world!"), Printfn(String "Bye world!"))
    ]
}
// End of sample Kobra modules

[<EntryPoint>]
let main argv = 
    try
        do evaluate moduleConditionalHelloWorld
        0
    with
    | Exception ex -> 
        do printfn "%A" ex
        1
