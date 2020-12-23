open Bttfrp

type action =
  | Left of int
  | Right of int
  | Insert of string
  | Del of int
  | Nothing

let empty_text = ("", "")

let left n = function
  | (ltext, rtext) ->
      let l = String.length ltext in
      if l <= n then ("", ltext ^ rtext)
      else (String.sub ltext 0 (l - n), String.sub ltext (l - n) n ^ rtext)

let right n = function
  | (ltext, rtext) ->
      let l = String.length rtext in
      if l <= n then (ltext ^ rtext, "")
      else (ltext ^ String.sub rtext 0 n, String.sub rtext n (l - n))

let del n = function
  | (ltext, rtext) ->
      let l = String.length ltext in
      if l <= n then ("", rtext) else (String.sub ltext 0 (l - n), rtext)

let insert s = function (ltext, rtext) -> (ltext ^ s, rtext)

let f_text user ptext =
  match user with
  | Left n ->
      left n ptext
  | Right n ->
      right n ptext
  | Del n ->
      del n ptext
  | Insert s ->
      insert s ptext
  | Nothing ->
      ptext

let f_pr_text = function
  | (ltext, rtext) ->
      Printf.printf "text %s|%s\n" ltext rtext

let f_pr_ptext = function
  | (ltext, rtext) ->
      Printf.printf "ptext %s|%s\n" ltext rtext

let f_pr_user ac =
  let s =
    match ac with
    | Left n ->
        "Left " ^ string_of_int n
    | Right n ->
        "Right " ^ string_of_int n
    | Del n ->
        "Del " ^ string_of_int n
    | Insert l ->
        "Insert " ^ l
    | Nothing ->
        ""
  in
  Printf.printf "ac : %s\n" s

let user_eq i v = match (i, v) with (Nothing, Nothing) -> true | _ -> false

let user = Bttfrp.Discrete.create ()

let user_c = Bttfrp.Continuous.complete_default ~default:Nothing user

let text_eq t t' =
  match (t, t') with
  | ((l, r), (l', r')) ->
      let b = l = l' && r = r' in
      (* Printf.printf "%B %s|%s , %s|%s\n" b l r l' r'; *)
      b

let (text, ptext) =
  Bttfrp.Continuous.fix ~fix_f:(fun text ->
      let ptext = Bttfrp.Continuous.previous ~origin:empty_text text in
      (Bttfrp.Continuous.map2 ~f:f_text user_c ptext, ptext))

let empty_all () =
  Bttfrp.empty text ;
  Bttfrp.empty ptext ;
  Bttfrp.empty user ;
  Bttfrp.empty user_c

let time t = Time.of_int t
