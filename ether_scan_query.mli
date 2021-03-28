module type ether_scan_query = sig
  (** [get_cure_price ()] is the pair (a, b) where a is the string
      describing the current Ethereum price in USD, and b is the string
      of an integer representing the current Epoch time. Errors: TBD *)
  val get_cur_price : unit -> string * string

  (* Exception esp_exception t *)
end
