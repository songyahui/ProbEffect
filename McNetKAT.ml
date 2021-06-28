(*
                -----9@ sw3 @10 -----
               /|\                 |
                |                  | 
                |                 \|/
                4                  6
                @                  @
T1@1 ------ 2@ sw1 @3 ------->>5@ sw2 @7 ------ 8@ T2

*)

type state =
  { sw  : int ref;
    pt  : int ref;
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
  else ()
  
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

let up (n:int) : int = 
  n;;


let p1_cap (s:state) : unit  = 
  if up 3 == 1 then (s.pt) := 3 
  else (s.pt) := 4 ;;


let p2_cap (s:state) : unit  = 
  (s.pt) := 7;;

let p3_cap (s:state) : unit  = 
  (s.pt) := 10;;



let p_cap (s:state) : unit =
  if !(s.sw) == 1 then  p1_cap s else
  if !(s.sw) == 2 then  p2_cap s
  else p3_cap s ;;
  

let main = model policy topo;


