(** Makes queries to retrieve information regarding current and
    historical prices of Ether *)

(** [get_cur_price ()] is the pair (a, b) where a is the string
    describing the current Ethereum price in USD, and b is the string of
    an integer representing the Epoch timestamp of the last price.
    @throws Query_Failed *)
val get_cur_price : unit -> string * string

(** Gets the historical high price at given date in format MM/DD/YYYY *)
val get_historical_high : string -> float

(** Gets the historical low price at given date in format MM/DD/YYYY *)
val get_historical_low : string -> float

(** if a query to etherscan.io fails, Query_Failed will be raised with
    details about what went wrong*)
exception Query_Failed of string
