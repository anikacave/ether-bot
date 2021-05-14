(** this file calculates indicator values from raw price data *)

(* a list of tuples of epoch time and USD price from csv_data_bot
   Requires: the time is ordered from newest to oldest *)

(*  AF: array of time & price pairs
    RI: all elements are sorted in chronological order. No duplicate times *)
type dataset = (int * float) array

(* checks that the dataset is in chronological order with no dupes*)
let rep_ok d : dataset = 
  for i=0 to Array.length d - 2 do
    if ((fst d.(i)) > (fst d.(i+1))) then
      failwith "dataset rep invariant violated in indicators.ml"
    else ()
  done;
  d

let readable_to_unix str =
  let splitcomma = String.split_on_char ',' str in
  if splitcomma = [] then None
  else
    let splitspace = String.split_on_char ' ' (List.hd splitcomma) in
    if splitspace = [] then None
    else
      let date = String.split_on_char '-' (List.hd splitspace) in
      if date = [] then None
      else
        let time = String.split_on_char ':' (List.nth splitspace 1) in
        if time = [] then None
        else
          let year = int_of_string (List.hd date) in
          let month = int_of_string (List.nth date 1) in
          let day = int_of_string (List.nth date 2) in
          let hour = int_of_string (List.hd time) in
          let minute = int_of_string (List.nth time 1) in
          let second = int_of_string (List.nth time 2) in
          let price = float_of_string (List.nth splitcomma 4) in
          let epoch =
            second + (60 * minute) + (3600 * hour) + (86400 * (day - 1))
          in
          let epochjan12021 = 1609477200 in
          Some (epochjan12021 + epoch, price)

(* parses a csv file and constructs dataset formatter describes how to
   parse each line into a tuple [from_csv formatter file_name] is a
   dataset from the file*)
let from_csv parsing_fcn file_name =
  (* TODO consider optimizing from O(2n) to O(n)*)
  let input_stream = open_in file_name in
  let rec scan acc =
    match
      try Some (input_line input_stream) with End_of_file -> None
    with
    | None ->
        Stdlib.close_in input_stream;
        acc
    | Some h -> (
        match parsing_fcn h with
        | None -> acc |> scan
        | Some x -> x :: acc |> scan)
  in
  scan [] |> Array.of_list

(** a sample fcn to pass to from_csv*)
let sample_fcn str =
  let vals = String.split_on_char ',' str in
  try
    Some
      ( List.nth vals 0 |> int_of_string,
        List.nth vals 1 |> float_of_string )
  with Failure _ -> None

(* constructs a dataset from a list of tuples *)
let from_tuple_list (lst : (int * float) list) : dataset = 
  let length = List.length lst in
  let arr = Array.make length (-1, -1.)
  in assert false

(* binary search index of the target in the dataset. If doesn't exist, the lower index
  from where it would have been*)
let index_of d target comp = 
  let rec helper low high = 
    let m = (low + high) / 2 in
    if low <= high then (
      if comp d.(m) target < 0 then helper (m+1) high
      else if comp d.(m) target > 0 then helper low (m-1)
      else m)
    else m
  in
  let low = 0 in 
  let high = (Array.length d) - 1 in
  helper low high

(* returns a subset of the dataset from [trim dataset begin end] is a
   dataset including datapoints between begin and end INCLUSIVE begin
   and end should be in epoch time *)
let rec trim (d : dataset) start finish : dataset =
  let comparator a b = b - (fst a) in
  let ind1 = index_of d start comparator in
  let ind2 = index_of d finish comparator in 
  let length = ind2 - ind1 + 1 in 
  let arr = Array.make length (-1 , -1.) in
  for i = 0 to length - 1 do
    arr.(i) <- d.(ind1 + i)
  done;
  arr
  
(* sum the prices in a dataset *)
let rec sum d = Array.fold_left (fun acc x -> snd x +. acc) 0. d

(* returns a list containing the average price within each period the
   length of the list should be num_intervals. Earlier averages are at the head *)
let rec avgs_in_period_list d period time =
  if fst d.(Array.length d - 1) > time then []
  else
    let trimmed = trim d (time - period) time in
    let recurse = avgs_in_period_list d period (time - period) in
    if Array.length trimmed = 0 then recurse
    else
      (sum trimmed /. float_of_int (Array.length trimmed)) :: recurse

(* [sma dataset period num_intervals time] is the SMA at time time of
   the dataset given the desired period and number of intervals to look
   back Require: time is in epoch time For example [sma dataset 86400 10
   1619582400] returns the 10 day daily average starting from April 28th
   2021 GMT (April 19th to 28th) *)
let rec sma d period num_intervals time =
  let trimmed_data = trim d (time - (period * num_intervals)) time in
  let averages = avgs_in_period_list trimmed_data period time in
  (* list of averages*)
  List.fold_left ( +. ) 0. averages
  /.  (float_of_int (List.length averages))

(** TODO make sma and ema more consistent by trimming *)

(* calculates the ema given the trimmed dataset period in seconds ie
   86400 is 1 day num_periods how many periods to look back smoothing
   constant*)
let rec ema d period num_periods smoothing =
  if num_periods <= 0 then 0.
  else if d = [] then 0.
  else
    let k = smoothing /. (float_of_int num_periods +. 1.) in
    (snd (List.hd d) *. k)
    +. (ema (List.tl d) period (num_periods - 1) smoothing *. (1. -. k))

type op = Low | High | Mean


let analyze d op =
  match op with
    | Low -> Array.fold_left min 0. d
    | High -> Array.fold_left max 0. d
    | Mean -> Array.fold_left (+.) 0. d 
      /. float_of_int (Array.length d)

let rec low_price d =
  let low = ref (-1.) in
  for i=0 to Array.length d - 1 do
    if d.(i) < !low then 
      low := d.(i)
    else ();
  done;
  low
let rec high_price d acc =
  match d with
  | [] -> acc
  | (_, price) :: t ->
      if price > acc then high_price t price else high_price t acc

(* [stoch d lookback time] is the stochastic oscillator looking back
   [lookback] seconds from time [time] Note: this method performs no
   smoothing Requires: the given dataset has enough information to
   lookback the desired amount and contains the time *)
let stoch (d : dataset) lookback time =
  assert false
  (* let trimmed = trim d (time - lookback) time in
  let c = snd (List.hd trimmed) in
  let low = low_price trimmed c in
  let high = high_price trimmed c in
  (c -. low) /. (high -. low) *. 100. *)

(* calculates adx Pushed to MS3*)
let adx d = failwith "unimplemented"

(* calculates macd by comparing 12 day vs 26 day ema*)
let macd d = ema d 12 12 2. -. ema d 26 26 2.

(* let sma_accessible file_name =
  let d = from_csv readable_to_unix file_name in
  if d = [] then 0. else sma d 86400 10 (fst (List.hd d)) *)

(* let sma_accessible file_name =
  let d = from_csv readable_to_unix file_name in
  sma d 3600 10 1610686740

let ema_accessible file_name =
  let d = from_csv readable_to_unix file_name in
  ema d 3600 10 2.

let stoch_accessible file_name =
  let d = from_csv readable_to_unix file_name in
  stoch d (List.length d) (fst (List.hd d))

let macd_accessible file_name =
  let d = from_csv readable_to_unix file_name in
  macd d *)
