type ('a, 'b, 'c) either = Left of 'a | Right of 'b | Middle of 'c



type term =
    | Const of float
    | Var   of string
    | Add   of term * term
    | Sub   of term * term


type literal = 
    | INT of int
    | STRING of string


type expression = 
    | Call of string * (literal list)
    | IfElse of (expression * expression) list * expression
    | Record of (string * expression) list
(* kleneen *)
    | Drop
    | Skip 
    | Test    of string * int
    | Assign    of string * int
    | Union     of expression * expression
    | Sequence  of expression * expression
    | Distribution  of (term * expression) list
    | Iteration of expression

type statement = string * (string list) * expression
    

type program = statement list

    
(* 



type test = 
    | Drop 
    | Skip
    | Test of field * int 
    | Disj of test * test
    | Conj of test * test
    | Neg of test 
   
type probNetKAT = 
    | Filter of test
    | Assign of field * int 
    | Union of probNetKAT * probNetKAT 
    | Sequence of probNetKAT * probNetKAT
    | Choice of probNetKAT * probNetKAT * float 
    | Iteration of probNetKAT
Source code *)