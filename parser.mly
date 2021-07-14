%{ open Ast %}
%{ open List %}

%token <string> STRING

%token <int> INTE

%token <string> VAR 

%token <float> FLOAT 

%token EQ COMMA LPAR RPAR ASSIGN SIMI
%token SKIP DROP IF ELSE THEN 
%token ProbModel LBRACK RBRACK AT BAR
%token  MINUS PLUS  
%token EMPTY CONCAT KLEENE GTEQ LTEQ GT LT CONJ
%token TRUE FALSE ENTIL NEGATION POWER CHOICE 
%token DISJ LBrackets RBrackets LEADTO
%token COLON True False
(*

%token     COMMA LBRACK RBRACK      
 
 EOF GT LT   GTEQ LTEQ   CONCAT 
%token VARKEY KLEENE NEW HIPHOP MODULE IN OUT 
%token EMIT AWAIT DO EVERY FORK PAR LOOP YIELD ABORT SIGNAL
%token IF HALT CONST LET HOP FUNCTION ASYNC IMPLY 
%token RETURN BREAK COLON ELSE TRY CATCH RUN
%token REQUIRE ENSURE  LSPEC RSPEC
*)

%token EOF

%start program ee
%type <(Ast.statement) list> program
%type <(Ast.entilment) list > ee

%%

ee: 
| EOF {[]}
| a = entailment SIMI r = ee { append [a] r }



program:
| EOF {[]}
| a = statement r = program { append [a] r }




statement:
| f = VAR  LPAR  p = parameter RPAR EQ expr = expression_shell {(f, p, expr)}



literal: 
| n = INTE {INT n}
| str = STRING {STRING str}

expression_shell:
| LPAR ex1 = expression_shell RPAR {ex1}
| ex1 = expression obj = maybeSeq {
  match obj with 
  | None -> ex1 
  | Some n -> Sequence (ex1, n)
  }

maybeSeq:
| {None}
| SIMI n = expression_shell {Some n}


record_helper:
| {[]}
| name = VAR EQ rest = expression SIMI more= record_helper {(name, rest)::more} 


distrubution:
| {[]}
| BAR f = term AT ex = expression_shell rest = distrubution {(f, ex):: rest} 

expression:
| SKIP {Skip} 
| DROP {Drop} 
| f = term AT ex = expression_shell rest = distrubution {Distribution ((f, ex)::rest)}
| ProbModel LBRACK 
r = record_helper 
RBRACK {
  Record r 
}

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
| ex = expression_shell {([], ex)}
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


term:
| str = VAR { Var str }
| f = FLOAT {Const f}
| LPAR r = term RPAR { r }
| LPAR a = term MINUS b = term RPAR {Sub (a, b )}

| LPAR a = term PLUS b = term RPAR {Add (a, b)}




pure:
| TRUE {TRUE}
| FALSE {FALSE}
| NEGATION LPAR a = pure RPAR {Neg a}
| LPAR r = pure RPAR { r }
| a = term GT b = term {Gt (a, b)}
| a = term LT b = term {Lt (a, b)}
| a = term GTEQ b = term {GtEq (a, b)}
| a = term LTEQ b = term {LtEq (a, b)}
| a = term EQ b = term {Eq (a, b)}
| a = pure CONJ b = pure {PureAnd (a, b)}
| a = pure DISJ b = pure {PureOr (a, b)}



es:
| EMPTY { Empty }
| LBRACK r = VAR RBRACK { Instant r }
| LPAR r = es RPAR { r }
| a = es DISJ b = es { Union(a, b) }
| a = es CONCAT b = es { Sequence(a, b) } 
| LPAR a = es POWER KLEENE RPAR{Kleene a}
| LBrackets f= term LEADTO e = es rest=pcase_help RBrackets {PCases ((f, e)::rest)}

pcase_help:
| {[]}
| BAR f= term LEADTO e = es rest=pcase_help {((f, e)::rest)}


effect:
| LPAR r = effect RPAR { r }
| a = pure  CONJ  b= es  {Effect (a, b)}
| a = effect  DISJ  b=effect  {Disj (a,b)}


expect :
| True {true}
| False {false}


entailment:
| lhs = effect   ENTIL   rhs = effect  COLON e = expect { (lhs, rhs, e)}
