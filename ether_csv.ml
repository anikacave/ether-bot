open Ether_scan_processing
open Csv

(* Filename type *)
type filename = string

(* TODO price will be from the get_price_time will ahve to format that *)
let row_header = "formatted time, price\n"

(* Checks if a row being appended/added to a CSV file is a valid row
   else will throw a TBD error -- also understood that this is terrible
   code right now but this will be updated to be a much more in-depth
   check of whether or not a csv row is valid*)
let is_valid_csv_row (row : string) =
  let elems = String.split_on_char ',' row in
  if List.length elems = 2 then false else false

(** Makes a formatted row of the csv file by calling functions from
    ether_scan_processing and converting into comma seperated values *)
let make_csv_row un =
  let price, time = get_price_time () in
  string_of_float price ^ "," ^ time ^ "\n"

(* Prints to CSV output channel if header is marked as true, then a
   header will be appended to the beginning of the channel otherwise it
   will be assumed that the file is already properly labelled *)
let csv_line channel header =
  let bytes2 =
    if header then row_header ^ make_csv_row () else make_csv_row ()
  in
  output_string channel bytes2

(** [create_csv ()] creates csv @ given filename (will be from current
    data availible and should be only one line long to include the most
    current information) *)
let create_csv (file : filename) =
  let buff = open_out file in
  csv_line buff true;
  Stdlib.close_out buff

(* Feeds information from one file to an output stream to copy the
   contents of a file into another *)
let rec copy_paste input_stream output_stream =
  match
    try Some (input_line input_stream) with End_of_file -> None
  with
  | None -> flush output_stream
  | Some h ->
      output_string output_stream (h ^ "\n");
      flush output_stream;
      copy_paste input_stream output_stream

(* updates the filename for any type of file (not just csv) so we can
   write to text files, etc, assumes that a file DOES have an extension
   TODO throw invalid file extension*)
let update_file_name (file : filename) =
  let elems = String.split_on_char '.' file in
  if List.length elems = 2 then
    List.nth elems 0 ^ "*." ^ List.nth elems 1
  else "TODO raise error"

(* [from_csv time] is the price of the ethereum at given [time]. If no
   such entry exists, raises (TBD) *)
let from_csv (flt : float) (file : filename) =
  let input_stream = open_in file in
  let rec scan un =
    match
      try Some (input_line input_stream) with End_of_file -> None
    with
    | None -> "TODO raise exception"
    | Some h ->
        let vals = String.split_on_char ',' h in
        if Float.of_string (List.nth vals 0) = flt then List.nth vals 1
        else scan ()
  in
  scan ()

(* [update_csv ()] appends the current data to the a specified csv file.
   Writes to a seperate file than the original one specified to avoid
   deleting all data from a csv file TODO return the name of the file
   that was written to*)
let safe_update_csv file =
  let input_stream = open_in file in
  let output_stream = open_out (update_file_name file) in
  copy_paste input_stream output_stream;
  csv_line output_stream false;
  Stdlib.close_out output_stream;
  Stdlib.close_in input_stream
