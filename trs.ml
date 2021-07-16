
open Pretty
open Ast


exception Foo of string

let rec string_of_terms (t:terms):string = 
  match t with
    Var name -> name
  | Const n -> string_of_float n
  | Add (t1, t2) -> (string_of_terms t1) ^ ("+") ^ (string_of_terms t2)
  | Sub (t1, t2) -> (string_of_terms t1) ^ ("-") ^ (string_of_terms t2)

  ;;

let rec string_of_es (es:es) :string = 
  match es with 
    Bottom -> "_|_"  
  | Empty -> "emp"
  | Instant str  -> "{" ^str ^"}"
  | Sequence (es1, es2) ->  "("^string_of_es es1 ^ " . " ^ string_of_es es2^")"
  | Kleene esIn -> "(" ^ string_of_es esIn ^ ")*" 
  | Union (es1, es2) -> string_of_es es1 ^ " \\/ " ^ string_of_es es2
  | PCases p_list -> "[" ^ 
    (
      let rec aux l : string =
        match l with 
        | [] -> ""
        | [(f, es)] -> string_of_terms f ^" -> " ^string_of_es es 
        | (f, es)::xs -> string_of_terms f ^" -> " ^string_of_es es ^ " | " ^ aux xs
      in aux p_list
    )
    ^ "]"

  ;;

let rec string_of_pure (p:pure):string = 
  match p with
    TRUE -> "true"
  | FALSE -> "false"
  | Gt (t1, t2) -> (string_of_terms t1) ^ ">" ^ (string_of_terms t2)
  | Lt (t1, t2) -> (string_of_terms t1) ^ "<" ^ (string_of_terms t2)
  | GtEq (t1, t2) -> (string_of_terms t1) ^ ">=" ^ (string_of_terms t2)
  | LtEq (t1, t2) -> (string_of_terms t1) ^ "<=" ^ (string_of_terms t2)
  | Eq (t1, t2) -> (string_of_terms t1) ^ "=" ^ (string_of_terms t2)
  | PureOr (p1, p2) -> "("^string_of_pure p1 ^ "\\/" ^ string_of_pure p2^")"
  | PureAnd (p1, p2) -> "("^string_of_pure p1 ^ "/\\" ^ string_of_pure p2^")"
  | Neg p -> "(!" ^ "(" ^ string_of_pure p^"))"
  ;; 

let rec string_of_eff (eff) : string = 
  match eff with 
  | Effect (p, es) -> string_of_pure p ^ "/\\" ^ string_of_es es 
  | Disj (eff1, eff2) -> string_of_eff eff1 ^ "\\/" ^ string_of_eff eff2
  ;;

let string_of_ee (lhs:eff) (rhs:eff) (e:bool) :string = 
  string_of_eff lhs ^" |- " ^string_of_eff rhs ^ ":" ^ string_of_bool e ^"\n"
  ;;



let inclusion (lhs:eff) (rhs:eff) (delta:hypotheses) : (binary_tree * bool  * hypotheses) = 
  print_string (string_of_ee lhs rhs true);
  (Leaf, true, delta)
  ;;

let printReport (lhs:eff) (rhs:eff) (e:bool) : string =
  let startTimeStamp = Sys.time() in
  let (tree, re, _) = inclusion lhs rhs [] in 
  let verification_time = "[Verification Time: " ^ string_of_float (Sys.time() -. startTimeStamp) ^ " s]\n" in
  let result = printTree ~line_prefix:"* " ~get_name ~get_children tree in
  let buffur = ( "===================================="^"\n" ^(string_of_ee lhs rhs e)^"\n[Result] " ^(if re then "Succeed\n" else "Fail\n") ^ verification_time^" \n\n"^ result)
  in buffur
  
  ;;

let () =
  let inputfile = (Sys.getcwd () ^ "/" ^ Sys.argv.(1)) in
(*    let outputfile = (Sys.getcwd ()^ "/" ^ Sys.argv.(2)) in
print_string (inputfile ^ "\n" ^ outputfile^"\n");*)
  let ic = open_in inputfile in
  try
      let lines =  (input_lines ic ) in
      let line = List.fold_right (fun x acc -> acc ^ "\n" ^ x) (List.rev lines) "" in
      let entailments = Parser.ee Lexer.token (Lexing.from_string line) in
            
      let result = List.map (fun (lhs, rhs, e) ->  printReport lhs rhs e ) entailments in 
      let final_result = List.fold_right (fun x acc -> acc  ^ x ^ "\n") ( result) "" in 

      print_string (final_result);


      flush stdout;                (* 现在写入默认设备 *)
      close_in ic                  (* 关闭输入通道 *)

    with e ->                      (* 一些不可预见的异常发生 *)
      close_in_noerr ic;           (* 紧急关闭 *)
      raise e                      (* 以出错的形式退出: 文件已关闭,但通道没有写入东西 *)

   ;;
