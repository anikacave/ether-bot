(** [create_csv ()] *)
val create_csv : unit -> unit

(** [from_csv time] is the [formatted_str_price_time] of the data
    written to the csv at [time]. If no such entry exists, raises (TBD) *)
val from_csv : float -> string

(** [update_csv ()] appends the current data to the log file. Creates an
    intermittent (?) file for safe writing*)
val safe_update_csv : unit -> unit
