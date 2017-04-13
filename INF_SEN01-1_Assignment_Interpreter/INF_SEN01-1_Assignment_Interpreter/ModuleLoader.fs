module ModuleLoader

open System
open System.IO

let loadModule path =
    File.ReadAllText(path)

let interpretModule path delimiter =
    (loadModule path).Split delimiter