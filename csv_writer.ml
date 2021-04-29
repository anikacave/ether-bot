open Ether_csv

(* have to open Ether_csv instead of main, opening main runs UI*)

(** [write_to_csv ()] is a script that continually writes current Ether
    prices to [filename], in usr_friendly format*)
let rec write_to_csv
    (filename : filename)
    (usr_friendly : readable_format) =
  if Sys.file_exists filename then safe_update_csv filename usr_friendly
  else create_csv filename usr_friendly;
  Unix.sleep 60;
  (* print_endline "in write_to_csv"; *)
  write_to_csv filename usr_friendly

(* will hopefully be able to add writing to a bot csv using anika's new
   methods in ether_csv.ml *)

let main = write_to_csv Sys.argv.(1) (bool_of_string Sys.argv.(2))

(* index this way b/c first argument is program name *)

(* true because we're writing to the human readable csv (ether_data.csv)*)
