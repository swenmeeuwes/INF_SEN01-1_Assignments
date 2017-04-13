module Parser

open Syntax
open System

exception Exception of string

let (|Int|_|) str =
   match System.Int32.TryParse(str) with
   | (true,int) -> Some(int)
   | _ -> None

let parseToken (tokenString: string) =
    match tokenString with
    | Int x -> IntegerType(x)
    | _     -> raise (Exception "")

let parseTokenSeq prevToken currToken nextToken =
    match currToken with
    | "+" -> Add(Value (parseToken prevToken), Value (parseToken nextToken))
