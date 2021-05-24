(** this file calculates indicator values from raw price data *)

(* represents the raw data to calculate indicators from *)
type dataset

(* set of indicators corresponding to a certain price change*)
type data_point = 
   {
   price_change : float;
   sma : float;
   stoch : float;
   adx : float;
   macd : float;
   }

(* represents an empty dataset *)
val empty_data : dataset

(** parses a csv file and constructs a dataset.
   [formatter] describes how to parse each line of the file
   [filename] is the name of the csv file to read
   [from_csv formatter filename] is dataset from the specified file*)
val from_csv : (string -> (int * float) option) -> string -> dataset

(** constructs a dataset from a list of tuples *)
val from_tuple_list : (int * float) list -> dataset

(**
   [trim dataset begin_time end_time] is a dataset including 
   datapoints between begin_time and end_time. If the dataset
   contains datapoints for begin and end, then they will be
   included. If not, the next most recent datapoint will be 
   included (round down). Both time parameters are in epoch time *)
val trim : dataset -> int -> int -> dataset

(** [sma dataset period num_periods time] is the sma
   [period] is the length of the period to look in seconds
   [num_periods] is the number of intervals to average
   [time] is where the function will look back from.
   For example, the range of relevant data will be between
   (time - period * num_periods..., time). For example 
   [sma dataset 86400 10 1619582400] returns the 10 day daily average starting from April 28th
   2021 GMT (April 19th to 28th)
   *)
val sma : dataset -> int -> int -> int -> float

(**[ema dataset period num_periods time smoothing]
   Similar documentation to sma but there is an optional
   smoothing argument. Default smoothing is 2.
*)
val ema : dataset -> int -> int -> int 
   -> ?smoothing: float -> float

(** [stoch data lookback time] is the stochastic oscillator 
   indicator with a lookback period of 14 days 
   and with closing time of ~11:59.
   Data between (time - lookback to time) will be analyzed *)
val stoch : dataset -> int -> int -> float

(** [adx data period time] is the average directional
   index of the period calculated with
   [period] in seconds and 
   looking back from [time] in epoch time*)
val adx : dataset -> int -> int -> float

(* [macd d period time] calculates the macd by 
  comparing the 12 period ema with the 26 period ema
  [period] is the desired period length in seconds
  time is when to look back from. Relevant data
  ranges from (time - 26*period) to time*)
val macd : dataset -> int -> int -> float

(** From the dataset, pairs the value of four indicators with 
   the price change after [delay] seconds
   and sampled every [period] seconds 
   [generate_datapoints <data> 3600 300] will
   generate an array that pairs the indicators with the price
   change after 1 hour, sampled every 5 minutes*)
val generate_datapoints : dataset -> int -> int -> data_point array

(** formats a data point into a human readable string*)
val string_of_data_point : data_point -> string

(** returns data_points with a change greater than the specified change
   [points_of_interest data delay period change]*)
val points_of_interest : dataset -> int -> int -> float -> data_point list


(** prints the specified dataset to the console.
   Mostly available as a debugging function *)
val print_data : dataset -> unit

(* Custom indicators coming soon!*)

(* for demo purposes *)
val sma_accessible : string -> float

val ema_accessible : string -> float

val stoch_accessible : string -> float

val macd_accessible : string -> float
