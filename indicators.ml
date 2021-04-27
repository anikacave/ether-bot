(** this file calculates indicator values from raw price data *)

(* a list of tuples of epoch time and USD price from csv_data_bot
   Requires: the time is ordered from newest to oldest *)

(* represents the raw data to calculate indicators from *)
type dataset = (int * float) list

(* parses a csv file and constructs dataset formatter describes how to
   parse each line into a tuple [from_csv formatter file_name] is a
   dataset from the file*)
let from_csv parsing_fcn file_name =
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
  scan []

let sample_fcn str =
  let vals = String.split_on_char ',' str in
  try
    Some
      ( List.nth vals 0 |> int_of_string,
        List.nth vals 1 |> float_of_string )
  with Failure _ -> None

(* constructs a dataset from a list of tuples *)
let from_tuple_list (lst : (int * float) list) : dataset = lst

(* returns a subset of the dataset from [trim dataset begin end] is a
   dataset including datapoints between begin and end inclusive *)
let rec trim (t : dataset) start finish : dataset =
  let filter_fun x = fst x > start && fst x <= finish in
  List.filter filter_fun t

let rec sum lst =
  match lst with [] -> 0. | (time, price) :: t -> price +. sum t

let rec avgs_in_period_list t period pd counter =
  if List.length t < pd then (0., counter)
  else
    let trim_list = trim t pd (pd + period) in
    let recurse =
      avgs_in_period_list t period (pd + period) (counter + 1)
    in
    if List.length trim_list = 0 then (fst recurse, counter)
    else
      ( (sum trim_list /. float_of_int (List.length trim_list))
        +. fst recurse,
        snd recurse )

(* [sma dataset period] is the SMA of the dataset given the desired
   period*)
let rec sma t period =
  let pair = avgs_in_period_list t period 0 1 in
  fst pair /. float_of_int (snd pair - 1)

let rec low_price t counter acc =
  match counter with
  | 13 -> acc
  | _ ->
      let counter_elem = snd (List.nth t counter) in
      if counter_elem < acc then low_price t (counter + 1) counter_elem
      else low_price t (counter + 1) acc

let rec high_price t counter acc =
  match counter with
  | 13 -> acc
  | _ ->
      let counter_elem = snd (List.nth t counter) in
      if counter_elem > acc then high_price t (counter + 1) counter_elem
      else high_price t (counter + 1) acc

(* [stoch data] is the stochastic oscillator (indicator) with lookback
   period of 14 days and with closing time of ~11:59*)
let stoch (t : dataset) =
  let c = snd (List.hd t) in
  let l14 = low_price t 1 c in
  let h14 = high_price t 1 c in
  (c -. l14) /. (h14 -. l14) *. 100.

(* calculates adx *)
let adx t = failwith "unimplemented"

(* calculates macd *)
let macd t = failwith "unimplemented"
