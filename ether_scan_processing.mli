(** [convert_cur_price ()] is the floating point number equivalent of
    the string input*)
val convert_cur_price : string -> float

(** [format_date()] returns a readable string in the form "Month_name
    day(st/nd/rd/th), year" from three integers month (0-11) day (0-31)
    and year (1970-inf.) *)
val format_date : int -> int -> int -> string

(** [format_time()] returns a readable string in the form "hh:mm:ss"
    from three integers 0-59 h,m,s *)
val format_time : int -> int -> int -> string

(** [convert_time_stamp ()] is the cleanly formatted string of the form
    "hh:mm:ss On month day, year" found from unix timestamp string and
    converted to a readable string *)
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

(** [csv_price_time ()] returns a CSV-friendly string of the form
    ["epoch time, price"]*)
val csv_price_time : unit -> string
