(** this file calculates indicator values from raw price data *)

(* a list of tuples of epoch time and USD price from csv_data_bot
   Requires: the time is ordered from newest to oldest *)

(* AF: array of time & price pairs RI: all elements are sorted in
   chronological order. No duplicate times *)
type dataset = (int * float) array
let empty_data = [||]


type op = 
  | Low 
  | High 
  | Mean 
  | Sum

(* checks that the dataset is in chronological order with no dupes*)
let rep_ok d : dataset =
  for i = 0 to Array.length d - 2 do
    if fst d.(i) > fst d.(i + 1) then
      failwith "dataset rep invariant violated in indicators.ml"
    else ()
  done;
  d

let analyze d op =
  let d = Array.map snd d in
  match op with
    | Low -> Array.fold_left min 0. d
    | High -> Array.fold_left max 0. d
    | Mean -> Array.fold_left (+.) 0. d 
      /. float_of_int (Array.length d)
    | Sum -> Array.fold_left (+.) 0. d 


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
        | Some x -> x :: acc |> scan )
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
  let length = List.length lst 
  in Array.init length (List.nth lst)

(* binary search index of the target in the dataset. If doesn't exist,
   the lower index from where it would have been*)
let index_of d target comp =
  let rec helper low high =
    let m = (low + high) / 2 in
    if low <= high then
      if comp d.(m) target < 0 then helper (m + 1) high
      else if comp d.(m) target > 0 then helper low (m - 1)
      else m
    else m
  in
  let low = 0 in
  let high = Array.length d - 1 in
  helper low high

(* returns a subset of the dataset from [trim dataset begin end] is a
   dataset including datapoints between begin and end INCLUSIVE begin
   and end should be in epoch time *)
let rec trim (d : dataset) start finish : dataset =
  let comparator a b = b - fst a in
  let ind1 = index_of d start comparator in
  let ind2 = index_of d finish comparator in
  let length = ind2 - ind1 + 1 in
  let arr = Array.make length (-1, -1.) in
  for i = 0 to length - 1 do
    arr.(i) <- d.(ind1 + i)
  done;
  arr |> rep_ok
  
(* returns a list containing the average price within each period the
   length of the list should be num_intervals. Earlier averages are at
   the head *)
let rec avgs_in_period_list d period time =
  if fst d.(Array.length d - 1) > time then []
  else 
    let trimmed = trim d (time - period) time in
    let recurse = avgs_in_period_list d period (time - period) in
    if Array.length trimmed = 0 then recurse
    else
      (analyze trimmed Mean ) :: recurse
      

(* [sma dataset period num_intervals time] is the SMA at time time of
   the dataset given the desired period and number of intervals to look
   back Require: time is in epoch time For example [sma dataset 86400 10
   1619582400] returns the 10 day daily average starting from April 28th
   2021 GMT (April 19th to 28th) *)
let rec sma d period num_intervals time =
  let trimmed_data = trim d (time - (period * num_intervals)) time in
  let averages = avgs_in_period_list trimmed_data period time in
  (* list of averages*)
  (List.fold_left ( +. ) 0. averages
  /.  (float_of_int (List.length averages)))


(* calculates the ema given the trimmed dataset period in seconds ie
   86400 is 1 day num_periods how many periods to look back smoothing
   constant*)
   (** TODO fix array to list conversions *)

let rec ema d period num_periods smoothing =
  let d = Array.to_list d in
  if num_periods <= 0 then 0.
  else if List.length d = 0 then 0.
  else
    let k = smoothing /. (float_of_int num_periods +. 1.) in
    (snd (List.hd d) *. k)
    +. (ema (Array.of_list (List.tl d)) period (num_periods - 1) smoothing *. (1. -. k))

(* [stoch d lookback time] is the stochastic oscillator looking back
   [lookback] seconds from time [time] Note: this method performs no
   smoothing Requires: the given dataset has enough information to
   lookback the desired amount and contains the time *)
let stoch (d : dataset) lookback time =
  let trimmed = trim d (time - lookback) time in
  let c = snd trimmed.(0) in
  let low = analyze trimmed Low in
  let high = analyze trimmed High in
  (c -. low) /. (high -. low) *. 100.

(* calculates adx Pushed to MS3*)
let adx d = failwith "unimplemented"

(* calculates macd by comparing 12 day vs 26 day ema*)
let macd d = ema d 12 12 2. -. ema d 26 26 2.

let sma_accessible file_name = 0.0

let ema_accessible file_name = 0.0

let stoch_accessible file_name = 0.0

let macd_accessible file_name = 0.0

(* let sma_accessible file_name = let d = from_csv readable_to_unix
   file_name in if d = [] then 0. else sma d 86400 10 (fst (List.hd d)) *)

(* let sma_accessible file_name = let d = from_csv readable_to_unix
   file_name in sma d 3600 10 1610686740

   let ema_accessible file_name = let d = from_csv readable_to_unix
   file_name in ema d 3600 10 2.

   let stoch_accessible file_name = let d = from_csv readable_to_unix
   file_name in stoch d (List.length d) (fst (List.hd d))

   let macd_accessible file_name = let d = from_csv readable_to_unix
   file_name in macd d *)
