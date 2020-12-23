open Bttfrp
open Editor

let t = ref Time.origin

let online = ref true

let pr_text = Event.Continuous.map f_pr_text text

let pr_ptext = Event.Continuous.map f_pr_ptext ptext

let print_v_t = function (l, r) -> "(" ^ l ^ "," ^ r ^ ")"

let print_v_u = function
  | Insert s ->
      "I : " ^ s
  | Del i ->
      "D : " ^ string_of_int i
  | Left i ->
      "L : " ^ string_of_int i
  | Right i ->
      "R : " ^ string_of_int i
  | Nothing ->
      "N "

let observe t =
  Printf.printf "\nobserving time %s\n%!" (Time.to_string t) ;
  try
    let txt = Event.observe text true t in
    Printf.printf "value \"%s|%s\"\n" (fst txt) (snd txt)
  with Not_found ->
    Printf.printf "No value at time %s\n%!" (Time.to_string t)

let refine ac =
  if not !online then (
    Printf.printf "time to refine : " ;
    t := Time.of_string (read_line ()) ) ;
  Event.refine user !t ac ;
  if !online then (
    observe !t ;
    t := Time.next !t )

let action c =
  match c with
  | 'l' ->
      Printf.printf "number to left  : " ;
      refine (Left (int_of_string (read_line ())))
  | 'r' ->
      Printf.printf "number to right : " ;
      refine (Right (int_of_string (read_line ())))
  | 'd' ->
      Printf.printf "number to del   : " ;
      refine (Del (int_of_string (read_line ())))
  | 'i' ->
      Printf.printf "string to add   : " ;
      refine (Insert (read_line ()))
  | 'o' ->
      Printf.printf "time to observe : " ;
      observe (Time.of_string (read_line ()))
  | 't' ->
      if !online then Printf.printf "time : " ;
      t := Time.of_string (read_line ()) ;
      observe !t
  | 'q' ->
      exit 0
  | _ ->
      ()

let rec init () =
  Printf.printf "online mode (y/n) : " ;
  match (read_line ()).[0] with
  | 'n' ->
      online := false
  | 'y' ->
      online := true
  | _ ->
      init ()

let help () =
  Printf.printf
    "les deux modes :\n\
     \t\t   Online  : chaque nouvelle user_action seras inscrit dans la \
     timeline au temps courant et l'observation de user se feras directement\n\
     \t\t   Offline : Il faut renseigner un temps pour chaque user_action, et \
     aucune observation n'est faite\n\
     \t\t   action  :\n\
     \t\t   'l' -> user_action = deplace le curseur à gauche\n\
     \t\t   'r' -> user_action = deplace le curseur à droite\n\
     \t\t   'd' -> user_action = supprime le character à gauche du curseur\n\
     \t\t   'i' -> user_action = ajoute s:string à gauche du curseur\n\
     \t\t   'o' -> observe user au temps t:Time.t\n\
     \t\t   't' -> change le temps courant pour t:Time.t //utile que dans le \
     mode online\n\
     \t\t   'q' -> exit\n"

let main () =
  help () ;
  init () ;
  while true do
    if !online then Printf.printf "\ncurrent time %s%!" (Time.to_string !t) ;
    Printf.printf "\naction : " ;
    action (read_line ()).[0]
  done

let _ = main ()
