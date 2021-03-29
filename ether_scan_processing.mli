(** *)
val convert_cur_price : string -> float

(** *)
val convert_time_stamp : string -> string

(** [get_price_time ()] is the pair (a, b) where a is the float
    describing the current Ethereum price in USD, and b is the string
    describing the date at which the Ether price was queried, in
    [hh:mm:ss day month year]. Errors: TBD *)
val get_price_time : unit -> float * string

(** [formatted_str_price_time ()] returns a cleanly formatted string of
    the form "Current Price: <price>\nAt Time: <time stamp>" (TBD), for
    printing to GUI*)
val formatted_str_price_time : unit -> string
