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

let action user previous_text =
  match user with
  | Left n ->
      left n previous_text
  | Right n ->
      right n previous_text
  | Del n ->
      del n previous_text
  | Insert s ->
      insert s previous_text
  | Nothing ->
      previous_text

let f_pr_text = function
  | (ltext, rtext) ->
      Printf.printf "text %s|%s\n" ltext rtext

let f_pr_ptext = function
  | (ltext, rtext) ->
      Printf.printf "previous_text %s|%s\n" ltext rtext

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

let user_continuous = Bttfrp.Continuous.complete_default ~default:Nothing user

let text_eq t t' =
  match (t, t') with
  | ((l, r), (l', r')) ->
      let b = l = l' && r = r' in
      (* Printf.printf "%B %s|%s , %s|%s\n" b l r l' r'; *)
      b

let (text, previous_text) =
  Bttfrp.Continuous.fix ~fix_f:(fun text ->
      let previous_text = Bttfrp.Continuous.previous ~origin:empty_text text in
      ( Bttfrp.Continuous.map2 ~f:action user_continuous previous_text,
        previous_text ))

let empty_all () =
  Bttfrp.empty text ;
  Bttfrp.empty previous_text ;
  Bttfrp.empty user ;
  Bttfrp.empty user_continuous

let time t = Time.of_int t
