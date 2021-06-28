(*
                -----9@ sw3 @10 -----
               /|\                 |
                |                  | 
                |                 \|/
                4                  6
                @                  @
T1@1 ------ 2@ sw1 @3 ------->>5@ sw2 @7 ------ 8@ T2

*)
open Float

type state =
  { sw  : int ref;
    pt  : int ref;
  }

type failure =
  { up3  : int;
    up4  : int;
  }

let policy (s:state) : unit=
  if !(s.sw) == 1 then 
    (
      (s.pt) := 3; 
    ) 
  else if !(s.sw) == 2 then 
    (
      (s.pt) := 7;
    )
  else ()
  ;;

let topo (s:state) :unit = 
  if !(s.sw)==1 && !(s.pt)==3 then 
    (
      (s.sw) := 2;   
      (s.pt) := 5
    )
  else if !(s.sw)==1 && !(s.pt)==4 then 
  (
    (s.sw) := 3;   
    (s.pt) := 9
  )
  else if !(s.sw)==3 && !(s.pt)==10 then 
  (
    (s.sw) := 2;   
    (s.pt) := 6
  )
  else ();;
  
let ingress:state = {sw = ref 1; pt = ref 2}

let egress:state = {sw = ref 2; pt = ref 7}

let compare s1 s2 : int = 
  if !(s1.sw) == !(s2.sw) && !(s1.pt) == !(s2.pt) then 0
  else -1
  ;;

let log s : unit = 
  print_string ("{sw=" ^ string_of_int (!(s.sw)) ^", pt=" ^ string_of_int (!(s.pt)) ^ "}\n")
  ;;

let model p t = 
  let s = ingress in 
  log s;
  p s; 
  log s; 
  while (compare s egress) != 0 do 
    (
      
      t s;
      log s; 
      p s;
      log s; 

    )
  done
;;
(********************************** 
Probabilistic reasoning
**********************************)

let f0 n : int =
  if n == 2 then 1 
  else (* n=3 *) 1 
  ;;

let f1 n : int = f0 n ;;

let prob (f:float): int = 
  Random.self_init ();
  let r = (Random.int 10) in 
  if of_int r < (f *. 10.0) then 1 else 0 ;;

  
let f2 n : int = 
  Random.self_init ();
  let r3 = (Random.int 10) < in 
  Random.self_init ();
  let r4 = (Random.int 10) in 

  if n == 2 then 
    if r3 < 8 then 
  else (* n == 3 *) prob (0.0)
  ;;


let up (n:int) (f) : int = 
  f n;;
  

let p1_cap (s:state) f : unit  = 
  if up 3 f == 1 then (s.pt) := 3 
  else (s.pt) := 4 ;;


let p2_cap (s:state) : unit  = 
  (s.pt) := 7;;

let p3_cap (s:state) : unit  = 
  (s.pt) := 10;;


let policy_cap (s:state) (f): unit =
  if !(s.sw) == 1 then  p1_cap s f else
  if !(s.sw) == 2 then  p2_cap s
  else p3_cap s ;;
  
let model_cap p t f: unit =
  let s = ingress in 
  log s;
  p s f; 
  log s; 
  while (compare s egress) != 0 do 
    (
      
      t s;
      log s; 
      p s f;
      log s; 

    )
  done
;;


let main = 
  model_cap policy_cap topo_cap f2; 

  (*
  model policy topo;
  model_cap policy_cap topo f0; 
  model_cap policy_cap topo f1; 
  model_cap policy_cap topo f2; 
  *)


