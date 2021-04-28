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
    | Some h -> begin
        match parsing_fcn h with
        | None -> acc |> scan
        | Some x -> 
          if x = (List.hd acc)
            then acc
          else x :: acc
        end
  in
  scan []

(** a sample fcn to pass to from_csv*)
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
   dataset including datapoints between begin and end INCLUSIVE
   begin and end should be in epoch time *)
let rec trim (d : dataset) start finish : dataset =
  let filter_fun x = fst x >= start && fst x <= finish in
  List.filter filter_fun d

(* sum the prices in a dataset *)
let rec sum d =
  List.fold_left (fun acc x -> snd x +. acc) 0. d

(* returns a list containing the average price within each period
    the length of the list should be num_intervals

   *)
let rec avgs_in_period_list d period time =
    if (List.length d) 
    |> List.nth d 
    |> fst > time then []
    else
    let trim_list = trim d (time - period) time in
    let recurse =
      avgs_in_period_list d period (time - period)
    in
    if List.length trim_list = 0 then recurse
    else
      ( (sum trim_list /. float_of_int (List.length trim_list))
        :: recurse)

(* [sma dataset period num_intervals time] is the SMA at time time of the dataset
   given the desired period and number of intervals to look back
   Require: time is in epoch time
   For example [sma dataset 86400 10 1619582400]
   returns the 10 day daily average starting from April 28th 2021 GMT (April 19th to 28th)
   *)
let rec sma d period num_intervals time =
  let trimmed_data = trim d (time - (period * num_intervals)) time in
  let averages = avgs_in_period_list trimmed_data period time in (** list of averages*)
  List.fold_left (+.) 0. averages
  |> (/.) (float_of_int (List.length averages))


  (** TODO make sma and ema more consistent by trimming *)

  (* calculates the ema given the trimmed dataset
  period in seconds ie 86400 is 1 day
  num_periods how many periods to look back
  smoothing constant*)
let rec ema d period num_periods smoothing =
  if num_periods <= 0 then 0.
  else
    let k = smoothing /. (float_of_int num_periods +. 1.) in
    (snd (List.hd d) *. k)
    +. (ema (List.tl d) period (num_periods - 1) smoothing) *. (1. -. k) 

let rec low_price d acc =
  match d with
  | [] -> acc
  | (_, price) :: t -> if price < acc then low_price t price
    else low_price t acc

let rec high_price d acc =
  match d with
  | [] -> acc
  | (_, price) :: t -> if price > acc then high_price t price
    else high_price t acc

(* [stoch d lookback time] is the stochastic oscillator 
    looking back [lookback] seconds
    from time [time]
    Note: this method performs no smoothing
    Requires: the given dataset has enough information to 
    lookback the desired amount and contains the time
   *)
let stoch (d : dataset) lookback time =
  let trimmed = trim d (time - lookback) time in
  let c = snd (List.hd trimmed) in
  let low = low_price trimmed c in
  let high = high_price trimmed c in
  (c -. low) /. (high -. low) *. 100.

(* calculates adx Pushed to MS3*)
let adx d = failwith "unimplemented"

(* calculates macd by comparing 12 day vs 26 day ema*)
let macd d = ema d 12 12 2. -. ema d 26 26 2.
