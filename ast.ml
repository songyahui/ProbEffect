type ('a, 'b, 'c) either = Left of 'a | Right of 'b | Middle of 'c



type terms =
    | Const of float
    | Var   of string
    | Add   of terms * terms
    | Sub   of terms * terms


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
    | Distribution  of (terms * expression) list
    | Iteration of expression

type statement = string * (string list) * expression
    

type program = statement list




type es =
    | Bottom
    | Empty
    | Instant  of string list
    | Sequence of es * es
    | Union    of es * es
    | Kleene   of es
    | PCases   of (terms * es) list


(*Arithimetic pure formulae*)
type pure = TRUE
          | FALSE
          | Gt of terms * terms
          | Lt of terms * terms
          | GtEq of terms * terms
          | LtEq of terms * terms
          | Eq of terms * terms
          | PureOr of pure * pure
          | PureAnd of pure * pure
          | Neg of pure

          
type eff = 
            Effect of pure * es
          | Disj of eff * eff


type entilment = EE of eff * eff

    
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