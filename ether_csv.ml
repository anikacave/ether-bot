open Ether_scan_processing
open Csv

(* Filename type *)
type filename = string

(* Checks if a row being appended/added to a CSV file is a valid row
   else will throw a TBD error *)
let is_valid_csv_row (row : string) = true

(** [create_csv ()] *)
let create_csv (file : filename) = ()

(** [from_csv time] is the [formatted_str_price_time] of the data
    written to the csv at [time]. If no such entry exists, raises (TBD) *)
let from_csv (flt : float) (file : filename) = "TODO"

(** [update_csv ()] appends the current data to the log file. Creates an
    intermittent (?) file for safe writing*)
let safe_update_csv un = ()
