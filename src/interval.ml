type t = bool Time.Timemap.t

let empty = Time.Timemap.empty

let valid time interval =
  let (prior_time, current_opt, _after_time) =
    Time.Timemap.split time interval
  in
  match current_opt with
  | Some is_valid ->
      (time, is_valid)
  | None ->
      if Time.Timemap.is_empty prior_time then (Time.of_int (-1), false)
      else Time.Timemap.max_binding prior_time

let clean interval =
  let filter_f time should_be_valid =
    let (time_prev, is_valid) = valid (Time.prev time) interval in
    time_prev < Time.origin || is_valid <> should_be_valid
  in
  Time.Timemap.filter filter_f interval

let already_invalidate interval time =
  if Time.Timemap.is_empty interval then false
  else
    let (max_time, is_valid) = Time.Timemap.max_binding interval in
    (not is_valid) && max_time <= time

let invalidate interval time =
  if not (already_invalidate interval time) then
    interval
    |> Time.Timemap.filter (fun time' _is_valid -> time' < time)
    |> Time.Timemap.add time false
    |> clean
  else interval

let validate interval time1 time2 =
  let (_time_prev2, is_valid) = valid time2 interval in
  interval
  |> Time.Timemap.filter (fun t _is_valid -> t < time1 || t > time2)
  |> Time.Timemap.add time1 true
  |> (fun interval ->
       if (not is_valid) && not (Time.Timemap.mem (Time.next time2) interval)
       then Time.Timemap.add (Time.next time2) false interval
       else interval)
  |> clean

let print interval name =
  let str =
    Time.Timemap.fold
      (fun t b acc ->
        Time.to_string t ^ " : " ^ string_of_bool b ^ " | " ^ acc)
      interval
      ""
  in
  Printf.printf "interval time %s : \n| %s\n" name str

let fold = Time.Timemap.fold
