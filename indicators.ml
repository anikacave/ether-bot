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
  let filter_fun x = fst x > start && fst x < finish in
  List.filter filter_fun t

let rec sum lst =
  match lst with [] -> 0. | (time, price) :: t -> price +. sum t

let rec trim_lists t period pd =
  match t with
  | [] -> []
  | _ -> trim t pd (pd + period) :: trim_lists t period (pd + period)

(* [sma dataset period] is the SMA of the dataset given the desired
   period*)
let sma t period = failwith "unimplemented"

(* [stoch data] is the stochastic oscillator (indicator) with lookback
   period of 14 days and with closing time of ~11:59*)
let stoch t = failwith "unimplemented"

(* calculates adx *)
let adx t = failwith "unimplemented"

(* calculates macd *)
let macd t = failwith "unimplemented"
