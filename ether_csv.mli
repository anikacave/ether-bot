(** The type filename *)
type filename = string

(** [create_csv ()] creates csv @ given filename (will be from current
    data availible and should be only one line long to include the most
    current information) *)
val create_csv : filename -> unit

(** [from_csv time] is the [formatted_str_price_time] of the data
    written to the csv at [time]. If no such entry exists, raises (TBD) *)
val from_csv : float -> filename -> string

(** [update_csv ()] appends the current data to the log file. Creates an
    intermittent (?) file for safe writing*)
val safe_update_csv : filename -> unit
