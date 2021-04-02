open Ether_scan_processing
open Ether_csv
open Ether_scan_query
open Unix

(* Here we should open Ether_csv, Ether_scan_processing,
   Ether_scan_query *)
exception File_not_found

(** An exception that may be raised when estimating how much you would
    have made*)
exception Est_price_exc of string

(** An exception when the user's input date is not of the form
    <mm/dd/yyyy>*)
exception Malformed_date of string

let filename = "ether_data.csv"

(* The initial price of Ether at the start of the session*)
let init_price = fst (get_price_time ())

(* Subtracts initial price from current price
   ([flt_how_much_would_have_made ()] is called upon "quit")*)
let flt_how_much_would_have_made () =
  fst (get_price_time ()) -. init_price

(* String to be printed of above method*)
let str_how_much_would_have_made () =
  let flt = flt_how_much_would_have_made () in
  let str = string_of_float (flt_how_much_would_have_made ()) in
  if flt < 0. then
    "If you had invested in Ether at the start of the session, you \
     would have lost $"
    ^ String.sub str 1 (String.length str - 1)
  else
    "If you had invested in Ether at the start of the session, you \
     would have made $" ^ str

(* ANSITerminal formatting*)
let print_fmt str = ANSITerminal.(print_string [ magenta ] str)

(** [update_create_csv ()] deals with making the csv if it does not
    exist, else just updates it. The name is: ether_data.csv*)
let update_create_csv un =
  if Sys.file_exists filename then safe_update_csv filename
  else create_csv filename

(** [print_cmds ()] prints the possible commands *)
let print_cmds () =
  ANSITerminal.(erase Screen);
  ANSITerminal.set_cursor 1 1;
  print_fmt "COMMANDS:\n";
  print_fmt "[0] - [quit]                             : quit program\n";

  print_fmt
    "[1] - [current price]                    : get the current USD \
     price of Ether\n";
  print_fmt
    "[2] - [open data]                        : opens the \
     ether_log.csv file in your system's default application\n";
  print_fmt
    "[3] - [price high today]                 : Ether high today\n";
  print_fmt
    "[4] - [price low today]                  : Ether low from today\n";
  print_fmt
    "[5 <mm/dd/yyyy>] - [price high mm/dd/yyyy]   : Ether low from \
     <mm/dd/yyyy>\n";
  print_fmt
    "[6 <mm/dd/yyyy>] - [price low mm/dd/yyyy]    : Ether high from \
     <mm/dd/yyyy>\n"

(** [open_data_csv] opens [ether_data.csv] if it exists, else it prints
    \"Can not present data\""*)
let open_data_csv () =
  if Sys.file_exists filename then (
    Unix.system ("open -a textedit " ^ filename);
    () )
  else print_fmt "Can not present data\n"

(** [reformat_user_timestamp s] is the csv-friendly timestamp, derived
    from input timestamp s. If s is not in the format <[m]m/[d]d/yyyy>,
    raises [Malformed_date] exception*)
let reformat_user_timestamp s =
  try
    match Stringext.full_split s '/' with
    | [ m; "/"; d; "/"; y ] ->
        format_date
          (int_of_string m - 1)
          (int_of_string d) (int_of_string y)
    | _ ->
        raise (Malformed_date "Input date not in form <[m]m/[d]d/yyyy>")
  with Invalid_date s ->
    raise (Malformed_date ("Incorrectly formated date: " ^ s))

(** [recieve_cmds ()] is a REPL that displays the possible commands,
    reroutes the user to another method, and quits upon "q"*)
let rec recieve_cmds () =
  print_string "> ";
  try
    match
      List.filter
        (fun s -> s <> " ")
        (Stringext.full_split (read_line ()) ' ')
    with
    | exception End_of_file -> ()
    | [ "0" ] | [ "q" ] | [ "Q" ] | [ "quit" ] | [ "Quit" ] ->
        ( try
            let str = str_how_much_would_have_made () in
            print_fmt (str ^ "\n")
          with Est_price_exc s -> print_fmt (s ^ "\n") );
        print_fmt "Quitting...\n"
    | [ "1" ] | [ "current"; "price" ] ->
        print_fmt ("updated file: " ^ update_create_csv () ^ "\n");
        print_fmt (formatted_str_price_time () ^ "\n") |> recieve_cmds
    | [ "2" ] | [ "open"; "data" ] -> open_data_csv () |> recieve_cmds
    | [ "3" ] | [ "price"; "high"; "today" ] ->
        print_fmt "command not currently available\n";
        recieve_cmds ()
    | [ "4" ] | [ "price"; "low"; "today" ] ->
        print_fmt "command not currently available\n";
        recieve_cmds ()
    | [ "5"; s ] | [ "price"; "high"; s ] -> (
        try
          let time = reformat_user_timestamp s in
          match time with
          | t ->
              print_fmt
                ( "You requested the high price from: " ^ t
                ^ ".\nCommand not currently available\n" );
              recieve_cmds ()
        with Malformed_date s ->
          print_fmt (s ^ "\n");
          recieve_cmds () )
    | [ "6"; s ] | [ "price"; "low"; s ] -> (
        try
          let time = reformat_user_timestamp s in
          match time with
          | t ->
              print_fmt
                ( "You requested the low price from: " ^ t
                ^ ".\nCommand not currently available\n" );
              recieve_cmds ()
        with Malformed_date s ->
          print_fmt (s ^ "\n");
          recieve_cmds () )
    | _ ->
        print_fmt
          "I could not understand your choice of command. Please try \
           again\n";
        recieve_cmds ()
  with Query_Failed s ->
    print_fmt (s ^ "\n");
    recieve_cmds ()

(** [yn_start ()] prompts user if they mean to enter the bot, calls
    [prompt_cmds ()] if "Y" and quits if "N". Repeats until desired
    input achieved*)
let rec yn_start () =
  print_string "(Y)es/(N)o >";
  match read_line () with
  | exception End_of_file -> ()
  | "N" | "n" | " n" | " N" | "yes" | "Yes" -> print_fmt "Quitting...\n"
  | "Y" | "y" | " y" | " Y" | "no" | "No" ->
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

(* Execute the ui. *)
let () = main ()
