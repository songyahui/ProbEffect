%{ open Ast %}
%{ open List %}

%token <string> STRING

%token <int> INTE

%token NEWLINE

%token <string> VAR 

%token EQ COMMA LPAR RPAR

(*

%token   SIMI  COMMA LBRACK RBRACK      
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
| f = VAR  LPAR  p = parameter RPAR EQ expr = expression {(f, p, expr)}



literal: 
| n = INTE {INT n}
| str = STRING {STRING str}

expression:
| {Unit}
| f = VAR LPAR  p = real_parameter RPAR   {Call (f, p)}


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


