open Ether_scan_processing
open Ether_csv
open Ether_scan_query
open Unix
open Wealth
open Analyze
open Indicators
(* open Indicators *)

(* Here we should open Ether_csv, Ether_scan_processing,
   Ether_scan_query *)
exception File_not_found

(** An exception when the user's input date is not of the form
    <mm/dd/yyyy>*)
exception Malformed_date of string

let readable_pid = ref 0

let unreadable_pid = ref 0

let filename = "ETH_1min_sample.tx"

let bot_filename = "ether_data_bot.csv"

(** [check_date mm_dd_yyyy] checks the date is valid, date input as
    <mm/dd/yyyy>. As in, not before 1900 and not after 2021, valid
    month, valid date, etc *)
let check_date mm_dd_yyyy =
  let cur_yr = Unix.(localtime (time ())).tm_year + 1900 in
  let cur_month = Unix.(localtime (time ())).tm_mon + 1 in
  let cur_day = Unix.(localtime (time ())).tm_mday in
  match
    List.map
      (fun i -> int_of_string i)
      (List.filter
         (fun s -> s <> "/")
         (Stringext.full_split mm_dd_yyyy '/'))
  with
  | [ month; day; year ] ->
      if
        year < 1900 || month < 1 || month > 12 || day < 1 || day > 31
        || (month == 4 || month == 6 || month == 9 || month == 11)
           && day > 30
        || (year mod 4 == 0 && month == 2 && day > 29)
        (* don't forget to test leap years! *)
        || (month == 2 && day > 28)
        || year > cur_yr
        || (year == cur_yr && (month > cur_month || day > cur_day))
      then raise (Malformed_date "invalid date")
      else mm_dd_yyyy
  | _ -> raise (Malformed_date "invalid date")

(* The initial price of Ether at the start of the session*)
let init_price = fst (get_price_time ())

(* Subtracts initial price from current price
   ([flt_how_much_would_have_made ()] is called upon "quit")*)
let flt_how_much_would_have_made () =
  fst (get_price_time ()) -. init_price

(* String to be printed of above method*)
let str_how_much_would_have_made () =
  let flt = flt_how_much_would_have_made () in
  let str = string_of_float flt in
  if flt < 0. then
    "If you had purchased one Ether at the start of the session, you \
     would have lost $"
    ^ String.sub str 1 (String.length str - 1)
    (* don't show negative sign just say "lost"*)
  else
    "If you had purchased one Ether at the start of the session, you \
     would have made $" ^ str

(* ANSITerminal formatting*)
let print_fmt str = ANSITerminal.(print_string [ magenta ] str)

(* NOW THIS IS IN csv_writer.ml : (** [update_create_csv ()] deals with
   making the csv if it does not exist, else just updates it. The name
   is: ether_data.csv*) let update_create_csv un = if Sys.file_exists
   filename then safe_update_csv filename true else create_csv filename
   true *)

(** [print_cmds ()] prints the possible commands *)
let print_cmds erase_screen =
  if erase_screen then (
    ANSITerminal.(
      erase Screen;
      set_cursor 1 1) )
  else ();
  print_fmt "HOME ~ COMMANDS:\n";
  print_fmt "[0] - [quit]                             : quit program\n";
  print_fmt
    "[1] - [current price]                    : get the current USD \
     price of Ether\n";
  print_fmt
    "[2] - [show wealth]                      : takes you to the \
     wealth management screen\n";
  print_fmt
    "[3] - [open data]                        : prints contents of \
     ether_data.csv to terminal\n";
  print_fmt
    "[4] - [open bot data]                    : prints contents of \
     ether_data_bot.csv to terminal\n";
  print_fmt
    "[5] - [price high today]                 : Ether high today\n";
  print_fmt
    "[6] - [price low today]                  : Ether low from today\n";
  print_fmt
    "[7 <mm/dd/yyyy>] - [price high mm/dd/yyyy]   : Ether low from \
     <mm/dd/yyyy>\n";
  print_fmt
    "[8 <mm/dd/yyyy>] - [price low mm/dd/yyyy]    : Ether high from \
     <mm/dd/yyyy>\n";
  print_fmt
    "[9] - [help]                             : Redisplay commands\n";
  print_fmt
    "[DEMO sma] / [DEMO ema] / [DEMO stoch] / [DEMO macd] : For \
     demonstration purposes only\n"

(** [open_data_csv] opens [ether_data.csv] in terminal if it exists,
    else it prints \"Can not present data\""*)
let open_data_csv filename =
  if Sys.file_exists filename then (
    Unix.system ("cat " ^ filename);
    () )
  else print_fmt "Can not present data\n"

(* this is not relevant right now b/c high and low from API (**
   [reformat_user_timestamp s] is the csv-friendly timestamp, derived
   from input timestamp s. If s is not in the format <[m]m/[d]d/yyyy>,
   raises [Malformed_date] exception*) let reformat_user_timestamp s =
   try match Stringext.full_split s '/' with | [ m; "/"; d; "/"; y ] ->
   format_date (int_of_string m - 1) (int_of_string d) (int_of_string y)
   | _ -> raise (Malformed_date "Input date not in form
   <[m]m/[d]d/yyyy>") with Invalid_date s -> raise (Malformed_date
   ("Incorrectly formated date: " ^ s)) *)

(* [quit_prog un] is the last method run in whole script; makes sure to
   kill the two processes spawned when the program starts up*)
let quit_prog un =
  let str = str_how_much_would_have_made () in
  print_fmt (str ^ "\n");
  kill !readable_pid 9;
  (*kill the bot writing to ether_csv*)
  kill !unreadable_pid 9;
  (*kill the bot writing to ether_bot.csv*)
  print_fmt "Quitting...\n"

(** [recieve_cmds ()] is a REPL that reroutes the user to another
    method, and quits upon "q"*)
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
        quit_prog ()
    | [ "1" ] | [ "current"; "price" ] ->
        (* print_fmt ("updated file: " ^ update_create_csv () ^ "\n"); *)
        (* SHOULD NOT BE UPDATING, THE BOT DOES THAT NOW!*)
        print_fmt (formatted_str_price_time () ^ "\n") |> recieve_cmds
    | [ "2" ] | [ "show"; "wealth" ] ->
        print_show_wealth true;
        (* the params are just temporary; later once wealth.ml is
           developed it'll be like "ether_amt_get", "ether_worth_get",
           "ether_spent_get"*)
        print_wealth_cmds ();
        (* don't erase screen *)
        recieve_wealth_cmds ()
    | [ "3" ] | [ "open"; "data" ] ->
        open_data_csv filename |> recieve_cmds
    | [ "4" ] | [ "open"; "bot"; "data" ] ->
        open_data_csv bot_filename |> recieve_cmds
    | [ "5" ] | [ "price"; "high"; "today" ] ->
        ( match high_today bot_filename with
        | exception TimestampNotFound ->
            print_fmt "No data from today\n"
        | time, price ->
            print_fmt
              ("Price high from today: " ^ string_of_float price ^ "\n")
        );
        recieve_cmds ()
    | [ "6" ] | [ "price"; "low"; "today" ] ->
        ( match low_today bot_filename with
        | exception TimestampNotFound ->
            print_fmt "No data from today\n"
        | time, price ->
            print_fmt
              ("Price low from today: " ^ string_of_float price ^ "\n")
        );
        recieve_cmds ()
    | [ "7"; s ] | [ "price"; "high"; s ] -> (
        match check_date s with
        (* query_failed caught below *)
        | s ->
            print_fmt
              ( "High price from " ^ s ^ ": "
              ^ string_of_float (get_historical_high s)
              ^ "\n" );
            recieve_cmds () )
    | [ "8"; s ] | [ "price"; "low"; s ] -> (
        match check_date s with
        (* query_failed caught below *)
        | s ->
            print_fmt
              ( "Low price from " ^ s ^ ": "
              ^ string_of_float (get_historical_low s)
              ^ "\n" );
            recieve_cmds () )
    | [ "9" ] | [ "help" ] | [ "Help" ] ->
        print_cmds false;
        recieve_cmds ()
    | [ "analyze" ] -> 
        print_show_analyze true;
        print_analyze_cmds ();
        recieve_analyze_cmds empty_data;
    | [ "DEMO"; indicator ] ->
        (* match indicator with | "sma" -> print_fmt ( "sma: " ^
           string_of_float (sma_accessible "ETH_1min_sample.txt") ^ "\n"
           ) |> recieve_cmds | "ema" -> print_fmt ( "ema: " ^
           string_of_float (ema_accessible "ETH_1min_sample.txt") ^ "\n"
           ) |> recieve_cmds | "stoch" -> print_fmt ( "stoch: " ^
           string_of_float (stoch_accessible "ETH_1min_sample.txt") ^
           "\n" ) |> recieve_cmds | "macd" -> print_fmt ( "macd: " ^
           string_of_float (macd_accessible "ETH_1min_sample.txt") ^
           "\n" ) |> recieve_cmds | _ -> *)
        print_fmt
          "could not understand. Please try again or type [help]";
        recieve_cmds ()
    | _ ->
        print_fmt
          "I could not understand your choice of command. Please try \
           again, or type [help]\n";
        recieve_cmds ()
  with
  | Query_Failed s ->
      print_fmt (s ^ "\n");
      recieve_cmds ()
  | Malformed_date s ->
      print_fmt (s ^ "\n");
      recieve_cmds ()

(** [print_show_wealth erase_screen] Displays the user's Ether wealth.
    The parameters are printed from the CSV. All calculations are done
    in [recieve_wealth_commands]*)
and print_show_wealth erase_screen =
  if erase_screen then (
    ANSITerminal.(erase Screen);
    ANSITerminal.set_cursor 1 1 )
  else ();
  print_fmt "WEALTH\n";
  print_fmt ("You own " ^ string_of_float (ether_own ()) ^ " Ether\n");
  print_fmt
    ( "Worth: "
    ^ string_of_float (ether_worth (just_cur_price ()))
    ^ "\n" );
  print_fmt ("Spent: " ^ string_of_float (ether_spent ()) ^ "\n")

(** [print_wealth_cmds un] Just displays the commands in the Wealth
    screen to the user*)
and print_wealth_cmds un =
  (* doesn't have a clear screen param b/c is always called after "help"
     or after showing the wealth *)
  print_fmt "COMMANDS\n";
  print_fmt "[0] - [quit]                             : Quit program\n";
  print_fmt
    "[1 <ff.ff>] - [buy <ff.ff>]              : Buy <ff.ff> Ether\n";
  print_fmt
    "[2 <ff.ff>] - [sell <ff.ff>]             : Sell <ff.ff> Ether\n";
  print_fmt
    "[3] - [restart]                          : Set \
     own/worth/spent/rev to 0\n";
  print_fmt
    "[4] - [home]                             : Return to home\n"

(** [recieve_wealth_cmds un] is a REPL that reads user's commands
    (specified in [print_wealth_cmds]) and redirects user to function
    that carries out that command*)
and recieve_wealth_cmds un =
  print_string "> ";
  match
    List.filter
      (fun s -> s <> " ")
      (Stringext.full_split (read_line ()) ' ')
  with
  | exception End_of_file -> ()
  | [ "0" ] | [ "q" ] | [ "Q" ] | [ "quit" ] | [ "Quit" ] ->
      quit_prog ()
  | [ "1"; flt ] | [ "buy"; flt ] -> (
      let amt_ether = float_of_string flt in
      (* since we bought, update the csv accordingly *)
      try
        wealth_bought amt_ether (just_cur_price ());
        print_show_wealth false;
        recieve_wealth_cmds ()
      with InvalidEtherAmount s ->
        print_fmt (s ^ "\n");
        recieve_wealth_cmds () )
  | [ "2"; flt ] | [ "sell"; flt ] -> (
      let amt_ether = float_of_string flt in
      try
        wealth_sold amt_ether (just_cur_price ());
        print_show_wealth false;
        recieve_wealth_cmds ()
      with
      | InvalidEtherAmount s ->
          print_fmt (s ^ "\n");
          recieve_wealth_cmds ()
      | InsufficientEtherInOwn s ->
          print_fmt (s ^ "\n");
          recieve_wealth_cmds () )
  | [ "3" ] | [ "restart" ] ->
      restart_wealth ();
      print_show_wealth false;
      recieve_wealth_cmds ()
  | [ "4" ] | [ "home" ] ->
      print_cmds true;
      recieve_cmds ()
  | [ "help" ] | [ "Help" ] ->
      print_fmt "[Help]:\n";
      print_wealth_cmds ();
      recieve_wealth_cmds ()
  | _ ->
      print_fmt
        "I could not understand your choice of command. Please try \
         again, or type [help]\n";
      recieve_wealth_cmds ()

(** [yn_start ()] prompts user if they mean to enter the bot, calls
    [prompt_cmds ()] if "Y" and quits if "N". Repeats until desired
    input achieved*)
let rec yn_start () =
  print_string "(Y)es/(N)o >";
  match read_line () with
  | exception End_of_file -> ()
  | "N" | "n" | " n" | " N" | "No" | "no" -> print_fmt "Quitting...\n"
  | "Y" | "y" | " y" | " Y" | "Yes" | "yes" ->
      print_cmds true;

      (* the true is for erasing the screen*)
      (* now get the bot to start working*)
      readable_pid :=
        create_process "./csv_writer.byte"
          [| "./csv_writer.byte"; "ether_data.csv"; "true" |]
          (* last param is if the data needs to be readable*)
          stdin stdout stderr;
      (* get another process to write to bot csv *)
      unreadable_pid :=
        create_process "./csv_writer.byte"
          [| "./csv_writer.byte"; "ether_data_bot.csv"; "false" |]
          (* last param is if the data needs to be readable*)
          stdin stdout stderr;
      (* start the wealth module (update refs, create log if it doesn't
         exist [should always exist])*)
      Wealth.initialize_wealth ();
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
