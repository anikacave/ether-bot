open Ether_scan_processing
open Csv
open Unix

(* Filename type *)
type filename = string

type readable_format = bool

type timestamp = float

type price = float

type comp =
  | Min
  | Max

exception TimestampNotFound

exception InvalidFileExtensionFormat

exception InvalidCSVFile

(* The current decided format of the csv file*)
let row_header = "formatted time,price\n"

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

(* helper function to find the position of an element in a list *)
let rec list_num list value num =
  match list with
  | [] -> raise TimestampNotFound
  | h :: t -> if h = value then num else list_num t value (num + 1)

(* Reads the header of the file and returns a pair which corresponds to
   the scope column number and the extreme column number (scope_col_num
   * ext_col_name)*)
let read_header input_stream scope_col_name ext_col_name =
  match
    try Some (input_line input_stream) with End_of_file -> None
  with
  | None ->
      Stdlib.close_in input_stream;
      raise InvalidCSVFile
  | Some h ->
      let head = String.split_on_char ',' h in
      (list_num head scope_col_name 0, list_num head ext_col_name 0)

(* Finds an extreme value based on a comparator and a column-type in the
   csv file. Returns the entire row where the extreme is found for a
   specified column. Takes in comparator < returns the smallest value in
   a range and > returns the largest value in a range. scope_col_name is
   the column that is being limited by min and max. ext_col_name is the
   column that will be used to provide comparisons. REQUIRES: the csv
   file has a header. REQUIRES: csv file is ordered by the scope
   parameter *)
let find_extreme_row
    (file : filename)
    comparator
    scope_col_name
    min
    ext_col_name =
  let input_stream = open_in file in
  let comp =
    match comparator with
    | Max -> fun a b -> a < b
    | Min -> fun a b -> a > b
  in
  let scope_col_num, ext_col_num =
    read_header input_stream scope_col_name ext_col_name
  in
  let rec scan past_min ext_val =
    match
      try Some (input_line input_stream) with End_of_file -> None
    with
    | None -> (
        Stdlib.close_in input_stream;
        match ext_val with
        | None ->
            Stdlib.close_in input_stream;
            raise TimestampNotFound
        | Some value -> value)
    | Some h -> (
        let vals =
          List.map
            (fun a -> Float.of_string a)
            (String.split_on_char ',' h)
        in
        if past_min = false then
          if List.nth vals scope_col_num < min then scan false None
          else scan true None
        else
          let current_val = List.nth vals ext_col_num in
          match ext_val with
          | None -> scan true (Some vals)
          | Some l ->
              let l_val = List.nth l ext_col_num in
              if comp l_val current_val then scan true (Some vals)
              else scan true (Some l))
  in
  scan false None

let epoch_time_24_hours_ago un = Unix.time () -. 86400.

let high_today (file : filename) =
  let row =
    find_extreme_row file Max "formatted time"
      (epoch_time_24_hours_ago ())
      "price"
  in
  (List.nth row 0, List.nth row 1)

let low_today (file : filename) =
  let row =
    find_extreme_row file Min "formatted time"
      (epoch_time_24_hours_ago ())
      "price"
  in
  (List.nth row 0, List.nth row 1)
