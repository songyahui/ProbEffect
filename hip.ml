
open Pretty
open Ast
open List


exception Foo of string

let string_of_literal (l:literal) : string = 
  match l with 
  | STRING str -> str
  | INT n -> string_of_int n 
  ;;

let rec string_of_expression expr : string = 
  let rec aux (li:string list) : string = 
    match li with 
    | [] -> ""
    | [x] -> x 
    | x::xs ->  x ^ "," ^ aux xs 
  in
  match expr with 
  | Unit -> ""
  | Call (na, parms) -> na ^ " (" ^ aux (List.map (fun a -> string_of_literal a) parms) ^ ") "
  | Union     (e1, e2) -> string_of_expression e1 ^ " \\/ " ^ string_of_expression e2
  | Sequence  (e1, e2) -> string_of_expression e1 ^ " /\\ " ^ string_of_expression e2

  
  | _ -> raise (Foo "song")
  (*
  | IfElse of (expression * expression) list
(* kleneen *)
  | Filter    of predicate 
  | Assign    of string * int
  | Choice    of (term * expression) list
  | Iteration of expression
  *)
  ;;

let string_of_statement statement: string=
  let rec aux li : string = 
    match li with 
    | [] -> ""
    | [x] -> x 
    | x::xs -> x ^ "," ^ aux xs 
  in
  let (name, params, expr) = statement in 
  name ^ 
  " (" ^ aux params ^ ") "
  ^ string_of_expression expr
  ;;


let rec string_of_program statements  :string =
  match statements with 
  | [] -> ""
  | x :: xs -> string_of_statement x ^ string_of_program xs
  ;;





let () =
  let inputfile = (Sys.getcwd () ^ "/" ^ Sys.argv.(1)) in
(*    let outputfile = (Sys.getcwd ()^ "/" ^ Sys.argv.(2)) in
print_string (inputfile ^ "\n" ^ outputfile^"\n");*)
  let ic = open_in inputfile in

  try
      let lines =  (input_lines ic ) in
      let line = List.fold_right (fun x acc -> acc ^ "\n" ^ x) (List.rev lines) "" in


      let progs = Parser.program Lexer.token (Lexing.from_string line) in

      print_string (string_of_program progs ) ; 
      flush stdout;                (* 现在写入默认设备 *)
      close_in ic                  (* 关闭输入通道 *)

    with e ->                      (* 一些不可预见的异常发生 *)
      close_in_noerr ic;           (* 紧急关闭 *)
      raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)

   ;;

