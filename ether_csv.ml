open Ether_scan_processing
open Csv

(* Filename type *)
type filename = string

(* TODO price will be from the get_price_time will ahve to format that *)
let row_header = "number, formatted date, formatted time, price"

(* Checks if a row being appended/added to a CSV file is a valid row
   else will throw a TBD error -- also understood that this is terrible
   code right now but this will be updated to be a much more in-depth
   check of whether or not a csv row is valid*)
let is_valid_csv_row (row : string) =
  let elems = String.split_on_char ',' row in
  if List.length elems = 4 then false else false

(** [create_csv ()] creates csv @ given filename (will be from current
    data availible and should be only one line long to include the most
    current information) *)
let create_csv (file : filename) = ()

(** [from_csv time] is the [formatted_str_price_time] of the data
    written to the csv at [time]. If no such entry exists, raises (TBD) *)
let from_csv (flt : float) (file : filename) = "TODO"

(** [update_csv ()] appends the current data to the log file. Creates an
    intermittent (?) file for safe writing*)
let safe_update_csv un = ()
