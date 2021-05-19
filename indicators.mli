(** this file calculates indicator values from raw price data *)

(* a list of tuples of epoch time and USD price from csv_data_bot
   Requires: the time is ordered from newest to oldest *)

(* represents the raw data to calculate indicators from *)

type dataset

val empty_data : dataset
(* parses a csv file and constructs a dataset formatter describes how to
   parse each line into a tuple [from_csv formatter file_name] is a
   dataset from the file*)

val from_csv : (string -> (int * float) option) -> string -> dataset

(* constructs a dataset from a list of tuples *)
val from_tuple_list : (int * float) list -> dataset

(* returns a subset of the dataset from [trim dataset begin_time
   end_time] is a dataset including datapoints between begin and end. If
   the dataset contains datapoints for begin and end, then they will be
   included. If not, the next most recent datapoint will be included
   (round down). {param} begin_time is in epoch time {param} end_time is
   in epoch time *)
val trim : dataset -> int -> int -> dataset

(* [sma dataset period num_periods time] *)
val sma : dataset -> int -> int -> int -> float

(* [stoch data lookback time] is the stochastic oscillator (indicator) with a lookback
   period of 14 days and with closing time of ~11:59*)
val stoch : dataset -> int -> int -> float

(* calculates adx *)
val adx : dataset -> float

(* calculates macd. comparing 12 day vs 26 day ema *)
val macd : dataset -> float

(* Custom indicators coming soon!*)

(* for demo purposes *)
val sma_accessible : string -> float

val ema_accessible : string -> float

val stoch_accessible : string -> float

val macd_accessible : string -> float
