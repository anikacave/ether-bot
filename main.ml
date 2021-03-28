open Ether_scan_processing
open Ether_csv
open Ether_scan_query
open Unix

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
  print_fmt
    "[current price]       : get the current USD price of Ether\n";
  print_fmt
    "[open data]           : opens the ether_log.csv file in your \
     system's default application\n";
  print_fmt "[price high mm/dd/yy] : Ether high from <mm/dd/yy>\n";
  print_fmt "[price low mm/dd/yy   : Ether low from <mm/dd/yy>\n";
  print_fmt "[price high today]    : Ether high from today\n";
  print_fmt "[price low today]     : Ether low from today\n";
  print_fmt "[quit]                : quit program\n"

(** [open_data_csv] opens [ether_data.csv] if it exists, else it prints
    \"Can not present data\""*)
let open_data_csv () =
  if Sys.file_exists "ether_data.csv" then (
    Unix.system "open -a textedit ether_data.csv";
    () )
  else print_fmt "Can not present data\n"

(** [reformat_user_timestamp s] is the csv-friendly timestamp, derived
    from input timestamp s*)
let reformat_user_timestamp s = s

(** [recieve_cmds ()] is a REPL that displays the possible commands,
    reroutes the user to another method, and quits upon "q"*)
let rec recieve_cmds () =
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "q" | "Q" | " q" | " Q" | "quit" | " Quit" | " quit" | "Quit" ->
      print_fmt "Quitting...\n"
  | "current price" ->
      print_fmt (formatted_str_price_time () ^ "\n") |> recieve_cmds
  | "open data" -> open_data_csv () |> recieve_cmds
  | s -> (
      match
        List.filter (fun s -> s <> " ") (Stringext.full_split s ' ')
      with
      | [ "price"; "high"; "today" ] ->
          print_fmt "command not currently available\n";
          recieve_cmds ()
      | [ "price"; "low"; "today" ] ->
          print_fmt "command not currently available\n";
          recieve_cmds ()
      | "price" :: "high" :: s -> (
          match reformat_user_timestamp s with
          | _ ->
              print_fmt "command not currently available\n";
              recieve_cmds () )
      | "price" :: "low" :: s -> (
          match reformat_user_timestamp s with
          | _ ->
              print_fmt "command not currently available\n";
              recieve_cmds () )
      | _ ->
          print_fmt
            "I could not understand your choice of command. Please try \
             again\n";
          recieve_cmds () )

(** [yn_start ()] prompts user if they mean to enter the bot, calls
    [prompt_cmds ()] if "Y" and quits if "N". Repeats until desired
    input achieved*)
let rec yn_start () =
  print_string "(Y)es/(N)o >";
  match read_line () with
  | exception End_of_file -> ()
  | "N" | "n" | " n" | " N" -> print_fmt "Quitting...\n"
  | "Y" | "y" | " y" | " Y" ->
      print_cmds ();
      recieve_cmds ()
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
