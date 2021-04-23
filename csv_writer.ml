open Ether_csv

(* have to open Ether_csv instead of main, opening main runs UI*)
let filename = "ether_data.csv"

(** [write_to_csv ()] is a script that continually writes current Ether
    prices to [filename]*)
let rec write_to_csv un =
  if Sys.file_exists filename then safe_update_csv filename
  else create_csv filename;
  Unix.sleep 30;
  print_endline "in write_to_csv";
  write_to_csv un

let main = write_to_csv ()
