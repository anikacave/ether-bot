open Indicators

let print_fmt str = ANSITerminal.(print_string [ magenta ] str)

(** a function to parse the 1 min file*)
let parsing_fcn1 str =
  let splitcomma = String.split_on_char ',' str in
  if splitcomma = [] then None
  else
    let splitspace = String.split_on_char ' ' (List.hd splitcomma) in
    if splitspace = [] then None
    else
      let date = String.split_on_char '-' (List.hd splitspace) in
      if date = [] then None
      else
        let time = String.split_on_char ':' (List.nth splitspace 1) in
        if time = [] then None
        else
          let year = int_of_string (List.hd date) in
          let month = int_of_string (List.nth date 1) in
          let day = int_of_string (List.nth date 2) in
          let hour = int_of_string (List.hd time) in
          let minute = int_of_string (List.nth time 1) in
          let second = int_of_string (List.nth time 2) in
          let price = float_of_string (List.nth splitcomma 4) in
          let epoch =
            second + (60 * minute) + (3600 * hour) 
            + (86400 * (day - 1)) in
          let epochjan12021 = 1609477200 in
          Some (epochjan12021 + epoch, price)

(** [print_show_wealth erase_screen] Displays the user's Ether wealth.
    The parameters are printed from the CSV. All calculations are done
    in [recieve_wealth_commands]*)
let rec print_show_analyze erase_screen =
    if erase_screen then (
      ANSITerminal.(erase Screen);
      ANSITerminal.set_cursor 1 1 )
    else ();
    print_fmt "Analysis Module\n";
  
  (** [print_analyze_cmds d] displays the commands in the Wealth
      screen to the user. The d argument is used to keep track of
      the dataset in question *)
and print_analyze_cmds d = 
(* doesn't have a clear screen param b/c is always called after "help"
    or after showing the wealth *)
print_fmt "COMMANDS\n";
print_fmt  "[home]                             : Return to home\n";
print_fmt
  "[open <file>]                           : Open <file> to analyze \n";
print_fmt
"[sma <period> <num_periods> <time>]     : Displays the simple mean average
\t looking back from <time>. All arguments should be in seconds \n";
print_fmt
"[help]                                      : Displays these commands again \n";


(** [recieve_wealth_cmds un] is a REPL that reads user's commands
  (specified in [print_wealth_cmds]) and redirects user to function
  that carries out that command*)
and recieve_analyze_cmds d =
(** TODO remove this*)
  let d = from_csv parsing_fcn1 "ETH_1min_sample.csv" in
  print_string "> ";
  match
    List.filter
      (fun s -> s <> " ")
      (Stringext.full_split (read_line ()) ' ')
  with
  | exception End_of_file -> ()
  | [ "0" ] | [ "home" ]  | [ "q" ] | 
    [ "Q" ] | [ "quit" ] | [ "Quit" ] ->
    ()
  | [ "open"; filename ] -> 
    let new_data = from_csv parsing_fcn1 filename in
    print_endline @@ "Loaded: " ^ filename;
    recieve_analyze_cmds new_data
  | [ "sma" ; period; num_periods; time] -> begin
      try 
        let avg = sma d (int_of_string period)
          (int_of_string num_periods) (int_of_string time)
        in avg |> string_of_float |> print_endline; 
        recieve_analyze_cmds d
      with 
      | Failure "int_of_string" -> (print_fmt
        "Please enter three integer valules \n";
        recieve_analyze_cmds d )
    end
  | [ "ema" ; period; num_periods; time] -> begin
    try 
      let avg = ema d (int_of_string period)
        (* smoothing constant is 2 by default in technical analysis*)
        (int_of_string num_periods) (int_of_string time) 
        ~smoothing:2.  
      in avg |> string_of_float |> print_endline; 
      recieve_analyze_cmds d
    with 
    | Failure "int_of_string" -> (print_fmt
      "Please enter three integer valules \n";
      recieve_analyze_cmds d )
  end

  | ["stoch"; lookback; time] -> begin
    try 
      stoch d (int_of_string lookback) (int_of_string time) 
      |> string_of_float 
      |> print_endline; 
      recieve_analyze_cmds d
    with 
    | Failure "int_of_string" -> (print_fmt
      "Usage: stoch <lookback> <time> as integers \n";
      recieve_analyze_cmds d )
  end
  | [ "help" ] | [ "Help" ] ->
      print_analyze_cmds ();
      recieve_analyze_cmds d
  (* a debugging command that won't be officially documented*) 
  | [ "print" ; "data"] -> 
    print_data d;
    recieve_analyze_cmds d
  | _ ->
      print_fmt
        "I could not understand your choice of command. Please try \
          again, or type [help]\n";
      recieve_analyze_cmds d