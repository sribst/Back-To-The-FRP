type 'a t = 'a Time.Timemap.t

let empty = Time.Timemap.empty

let add cache time occ = Time.Timemap.add time occ cache

let occurence time cache =
  try Some (time, Time.Timemap.find time cache) with Not_found -> None

let last_occurence time cache =
  let (l_cache, occ_opt, _r_cache) = Time.Timemap.split time cache in
  match occ_opt with
  | Some occ ->
      Some (time, occ)
  | None ->
      if Time.Timemap.is_empty l_cache then None
      else Some (Time.Timemap.max_binding l_cache)

let filter = Time.Timemap.filter

let print cache pp name =
  Printf.printf "\nValue %s :\n%!" name ;
  Time.Timemap.iter
    (fun time occ ->
      Printf.printf "| (%d : %s) %!" (Time.to_int time) (pp occ))
    cache ;
  Printf.printf "|\n\n%!"
