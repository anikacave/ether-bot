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
   (time - period * num_periods..., time)
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

(** calculates adx *)
val adx : dataset -> float

(** calculates macd. comparing 12 day vs 26 day ema *)
val macd : dataset -> int -> float

(** From the dataset, pairs the value of four indicators with 
   the price change after [delay] seconds
   and sampled every [period] seconds 
   [generate_datapoints <data> 3600 300] will
   generate an array that pairs the indicators with the price
   change after 1 hour, sampled every 5 minutes*)
val generate_datapoints : dataset -> int -> int -> data_point array

(** prints the specified dataset to the console.
   Mostly available as a debugging function *)
val print_data : dataset -> unit

(* Custom indicators coming soon!*)

(* for demo purposes *)
val sma_accessible : string -> float

val ema_accessible : string -> float

val stoch_accessible : string -> float

val macd_accessible : string -> float
