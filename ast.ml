(* Source code *)

type literal = 
    | INT of int
    | STRING of string
    | BOOL of bool
    
type expression = 
    | Unit
    | Variable of string
    | Literal of literal
   