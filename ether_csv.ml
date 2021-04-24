open Ether_scan_processing
open Csv

(* Filename type *)
type filename = string

type readable_format = bool

exception TimestampNotFound

exception InvalidFileExtensionFormat

(* The current decided format of the csv file*)
let row_header = "formatted time, price\n"

(* Checks if a row being appended/added to a CSV file is a valid row
   else will throw a TBD error *)
let is_valid_csv_row (row : string) =
  let elems = String.split_on_char ',' row in
  List.length elems = 2

(* NOTE: 2 csvs --> new csv functions to be used*)

(** Makes a formatted row of the csv file by calling functions from
    ether_scan_processing and converting into comma seperated values *)
let make_csv_row un = csv_readable_price_time () ^ "\n"

(** Makes a formatted row of the csv file by calling functions from
    ether_scan_processing and converting into comma seperated values *)
let make_csv_bot_row un = csv_bot_price_time () ^ "\n"

(* Prints to CSV output channel if header is marked as true, then a
   header will be appended to the beginning of the channel otherwise it
   will be assumed that the file is already properly labelled *)
let csv_line channel header (usr_friendly : readable_format) () =
  let format () =
    match usr_friendly with
    | true -> make_csv_row ()
    | false -> make_csv_bot_row ()
  in
  let bytes2 = if header then row_header ^ format () else format () in
  output_string channel bytes2

(** [create_csv file] creates csv @ given filename (will be from current
    data availible and should be only one line long to include the most
    current information) *)
let create_csv (file : filename) (usr_friendly : readable_format) =
  let buff = open_out file in
  csv_line buff true usr_friendly ();
  Stdlib.close_out buff;
  file

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
   else throws InvalidFileExtensionFormat*)
let update_file_name (file : filename) =
  let elems = String.split_on_char '.' file in
  if List.length elems = 2 then
    List.nth elems 0 ^ "*." ^ List.nth elems 1
  else raise InvalidFileExtensionFormat

(* [from_csv time file] is the price of the ethereum at given [time]. If
   no such entry exists, raises TimeStampNotFound *)
let from_csv (flt : float) (file : filename) =
  let input_stream = open_in file in
  let rec scan un =
    match
      try Some (input_line input_stream) with End_of_file -> None
    with
    | None ->
        Stdlib.close_in input_stream;
        raise TimestampNotFound
    | Some h ->
        let vals = String.split_on_char ',' h in
        if Float.of_string (List.nth vals 0) = flt then (
          let close un = Stdlib.close_in input_stream in
          close ();
          List.nth vals 1)
        else scan ()
  in
  scan ()

(* [update_csv filename] appends the current data to the a specified csv
   file. Writes to a seperate file than the original one specified to
   avoid deleting all data from a csv file. returns the name of the new
   file that was written to*)
let safe_update_csv (file : filename) (usr_friendly : readable_format) =
  let input_stream = open_in file in
  let new_file_name = update_file_name file in
  let output_stream = open_out new_file_name in
  copy_paste input_stream output_stream;
  csv_line output_stream false usr_friendly ();
  Stdlib.close_out output_stream;
  Stdlib.close_in input_stream;
  Sys.rename file (new_file_name ^ "temp");
  Sys.rename new_file_name file;
  Sys.rename (new_file_name ^ "temp") new_file_name;
  file
