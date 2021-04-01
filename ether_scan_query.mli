(** [get_cure_price ()] is the pair (a, b) where a is the string
    describing the current Ethereum price in USD, and b is the string of
    an integer representing the current Epoch time. Errors: TBD *)
val get_cur_price : unit -> string * string

(* if a query to etherscan.io fails, Query_Failed will be raised 
    with details about what went wrong*)
exception Query_Failed of string