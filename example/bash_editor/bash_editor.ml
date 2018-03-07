open Bttfrp
open Editor
       
let t = ref T.origin
let online = ref true

let pr_text = C.map f_pr_text text

let pr_ptext = C.map f_pr_ptext ptext

let print_v_t = function
    (l,r)-> "("^l^","^r^")"
			  
let print_v_u = function
  | Insert s ->  "I : " ^ s
  | Del i    ->  "D : " ^ string_of_int i
  | Left i   ->  "L : " ^ string_of_int i
  | Right i  ->  "R : " ^ string_of_int i
  | Nothing  ->  "N "

let print_vt ()=
  S.print_time user "user ";
  S.print_value user  print_v_u "user ";
  S.print_time ptext "ptext ";
  S.print_value ptext print_v_t "ptext ";
  S.print_time text "text ";
  S.print_value text  print_v_t "text  " 
	       
let observe t =
  (* print_vt (); *)
  Printf.printf "\nobserving time %s\n%!" (T.to_string t);
  try ignore (S.observe text t)
  with Not_found -> Printf.printf "No value at time %s\n%!" (T.to_string t)
				  
let refine ac =
  if not !online then
    begin
      Printf.printf "time to refine : ";
      t := T.of_string (read_line ())
    end;
  S.refine user !t ac ;
  if !online then
    begin
      observe !t;
      t := T.next !t
    end

let action c =
  match c with
  | 'l' -> Printf.printf "number to left  : " ; refine (Left (int_of_string (read_line ())))
  | 'r' -> Printf.printf "number to right : " ; refine (Right (int_of_string (read_line ())))
  | 'd' -> Printf.printf "number to del   : " ; refine (Del (int_of_string (read_line ())))
  | 'i' -> Printf.printf "string to add   : " ; refine (Insert (read_line ()))
  | 'o' -> Printf.printf "time to observe : " ; observe (T.of_string (read_line ()))
  | 't' -> if !online then
	     Printf.printf "time : "; t := T.of_string (read_line ()); observe !t
  | 'q' -> exit 0
  | _   -> ()

let rec init () =
  Printf.printf "online mode (y/n) : ";
  match (read_line ()).[0] with
  | 'n' -> online := false
  | 'y' -> online := true
  |  _  -> init ()


let help () =
  Printf.printf   "les deux modes :
		   Online  : chaque nouvelle user_action seras inscrit dans la timeline au temps courant et l'observation de user se feras directement
		   offline : Il faut renseigner un temps pour chaque user_action, et aucune observation n'est faite
		   action  :
		   'l' -> user_action = deplace le curseur à gauche
		   'r' -> user_action = deplace le curseur à droite
		   'd' -> user_action = supprime le character à gauche du curseur
		   'i' -> user_action = ajoute s:string à gauche du curseur
		   'o' -> observe user au temps t:T.t
		   't' -> change le temps courant pour t:T.t //utile que dans le mode online
		   'q' -> exit\n"
	     
let main () =
  help ();
  init ();
  while true do
    if !online then Printf.printf "\ncurrent time %s%!" (T.to_string !t);
    Printf.printf "\naction : ";
    action (read_line ()).[0];
  done
    
let _ =
  main ()
