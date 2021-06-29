type predicate = 
      Drop
    | Skip 
    | EQ of string * int
    | Conj of predicate * predicate
    | Disj of predicate * predicate
    | Neg  of predicate 

type term =
  | Const of float
  | Var   of string
  | Add   of term * term
  | Sub   of term * term

type expression = 
    | Filter    of predicate 
    | Assign    of string * int
    | Union     of expression * expression
    | Sequence  of expression * expression
    | Choice    of (term * expression) list
    | Iteration of expression

type literal = 
    | INT of int
    | STRING of string

type statement = 
    | Unit
    | Expr of expression
    | FunctionDeclear of string * string list * statement
    | Call of string * literal list
    | Rules of (expression * expression) list
    

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