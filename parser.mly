%{ open Ast %}
%{ open List %}

%token <string> STRING

%token <int> INTE

%token NEWLINE

%token <string> VAR 

%token EQ COMMA LPAR RPAR ASSIGN SIMI
%token SKIP DROP IF ELSE THEN

(*

%token     COMMA LBRACK RBRACK      
%token  MINUS PLUS   
 EOF GT LT   GTEQ LTEQ   CONCAT 
%token VARKEY KLEENE NEW HIPHOP MODULE IN OUT 
%token EMIT AWAIT DO EVERY FORK PAR LOOP YIELD ABORT SIGNAL
%token IF HALT CONST LET HOP FUNCTION ASYNC IMPLY 
%token RETURN BREAK COLON ELSE TRY CATCH RUN
%token REQUIRE ENSURE  LSPEC RSPEC
*)

%token EOF

%start program
%type <(Ast.statement) list> program


%%

program:
| EOF {[]}
| a = statement r = program { append [a] r }




statement:
| f = VAR  LPAR  p = parameter RPAR EQ expr = expression_shell {(f, p, expr)}



literal: 
| n = INTE {INT n}
| str = STRING {STRING str}

expression_shell:
| ex1 = expression obj = maybeSeq {
  match obj with 
  | None -> ex1 
  | Some n -> Sequence (ex1, n)
  }

maybeSeq:
| {None}
| SIMI n = expression_shell {Some n}



expression:
| SKIP {Skip} 
| DROP {Drop} 
| IF ex1 = expression_shell THEN ex2 = expression_shell ELSE rest= more_ifelse {
  let (more_if, final) = rest in 
  IfElse (List.append [(ex1, ex2)] more_if, final)

}

| LPAR ex = expression RPAR {ex}
| f = VAR e = expr_rest {
  match e with 
  | Left p -> Call (f, p)
  | Right n -> Assign (f, n)
  | Middle n -> Test (f, n)

}

more_ifelse:
| ex = expression {([], ex)}
| IF ex1 = expression_shell THEN ex2 = expression_shell rest= more_ifelse {
  let (more_if, final) = rest in 
  (List.append [(ex1, ex2)] more_if, final)
}




expr_rest: 
| LPAR  p = real_parameter RPAR   {Left p}
| ASSIGN n = INTE {Right n}
| EQ n = INTE {Middle n}


real_param:
| l = literal {l}


real_parameter:
| {[]}
| p = real_param obj = real_maybeNext {
  match obj with 
  | None -> [p]
  | Some obj -> p::obj}

real_maybeNext:
| {None}
| COMMA v = real_parameter {Some v}




param:
| str = VAR {str}


parameter:
| {[]}
| p = param obj = maybeNext {
  match obj with 
  | None -> [p]
  | Some obj -> p::obj}

maybeNext:
| {None}
| COMMA v = parameter {Some v}


