(** this file calculates indicator values from raw price data *)

(* a list of tuples of epoch time and USD price from csv_data_bot 
    Requires: the time is ordered from newest to oldest *)

(* represents the raw data to calculate indicators from *)
type dataset = int * float list


(* parses a csv file and constructs dataset 
    formatter describes how to parse each line into a tuple
    [from_csv formatter file_name] is a dataset from the file*)
val from_csv : (string -> (int * float) option) -> string -> dataset

(* constructs a dataset from a list of tuples *)
val from_tuple_list : (int * float list) -> dataset

(* returns a subset of the dataset from 
    [trim dataset begin end] is a dataset including datapoints 
    between begin and end inclusive *)
val trim : dataset -> int -> int -> dataset

(* [sma dataset period] is the SMA of the dataset 
    given the desired period*)
val sma : dataset -> int -> float

(* [stoch data] is the stochastic oscillator (indicator)
    with lookback period of 14 days and with closing time of ~11:59*)
val stoch : dataset -> float

(* calculates adx *)
val adx : dataset -> float

(* calculates macd *)
val macd : dataset -> float

(* Custom indicators coming soon!*)