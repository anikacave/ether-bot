open Ether_scan_processing
open Ether_csv
open Ether_scan_query

(* Here we should open Ether_csv, Ether_scan_processing,
   Ether_scan_query *)
exception File_not_found

let print_fmt str =
  ANSITerminal.(print_string [ magenta; on_white ] str)

(** [print_cmds ()] prints the possible commands *)
let print_cmds () =
  ANSITerminal.(erase Screen);
  ANSITerminal.set_cursor 1 1;
  print_fmt "COMMANDS:\n";
  print_fmt "[current price] : get the current USD price of Ether\n";
  print_fmt
    "[open data]     : opens the ether_log.csv file in your system's \
     default application\n";
  print_fmt "[quit]          : quit program\n"

(** [recieve_cmds ()] is a REPL that displays the possible commands,
    reroutes the user to another method, and quits upon "q"*)
let rec recieve_options () =
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "q" | "Q" | " q" | " Q" | "quit" | " Quit" | " quit" | "Quit" ->
      print_fmt "Quitting...\n"
  | "current price" -> print_string (formatted_str_price_time () ^ "\n")
  | "open data" -> failwith "TODO"
  | _ ->
      print_fmt
        "I could not understand your choice of command. Please try again\n";
      recieve_options ()

(** [yn_start ()] prompts user if they mean to enter the bot, calls
    [prompt_cmds ()] if "Y" and quits if "N". Repeats until desired
    input achieved*)
let rec yn_start () =
  print_string "(Y)es/(N)o >";
  match read_line () with
  | exception End_of_file -> ()
  | "N" | "n" | " n" | " N" -> print_endline "Quitting..."
  | "Y" | "y" | " y" | " Y" ->
      print_cmds ();
      recieve_options ()
  | _ ->
      print_fmt
        "Please respond with \"Y\" for yes, or \"N\" for no (quit)\n";
      yn_start ()

(** [main ()] calls [yn_start ()] and begins execution*)
let main () =
  ANSITerminal.(erase Screen);
  ANSITerminal.set_cursor 1 1;
  print_fmt "Welcome to our Ether Bot!";
  print_fmt "\nWould you like to launch the bot?";
  print_endline "";
  yn_start ()

(* Execute the game engine. *)
let () = main ()
