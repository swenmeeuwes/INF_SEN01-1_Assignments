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

let moduleDeclareHelloWorld = {
    variables = Map.empty
    statements = [
        Declaration ("someVariable", (String "Hello world!"))
        Printfn (Variable "someVariable")
        Printfn (String "Done!")
    ]
}

let moduleIterative = {
    variables = Map.empty
    statements = [
        Declaration ("counter", (Integer 1))
        While(LessThan(Variable "counter", Integer 10), Block([Printfn(Variable "counter"); Declaration("counter", Add(Variable "counter", Integer 1))]))
    ]
}

let moduleExtremelyDifficultComputation = {
    variables = Map.empty
    statements = [
        Declaration ("x", (Integer 1))
        Declaration ("y", (Integer 2))
        Declaration ("result", Add ((Variable "x"), (Variable "y")))
        Printfn (Variable "result")
        Printfn (String "Done!")
    ]
}
// End of sample Kobra modules

[<EntryPoint>]
let main argv = 
    try
        do evaluate moduleIterative
        0 // Exit correctly
    with
    | Interpreter.Exception ex -> 
        do printfn "%A" ex
        1 // Exit with exception
