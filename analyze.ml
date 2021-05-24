open Indicators

exception Bad_command of string

(** [epoched_reversed_list lst multiplier] creates a string in the rigth
    order out of [lst] and multiplies its integer equivalent by
    [multiplier]*)
let epoched_reversed_list lst multiplier =
  try
    let rev_tl = List.rev lst in
    let str_time =
      List.fold_right (fun x acc -> String.make 1 x ^ acc) rev_tl ""
    in
    let time = int_of_string str_time in
    let epoch_hours = time * multiplier in
    epoch_hours
  with Failure s ->
    raise
      (Bad_command
         "time must be integer followed by \"h\", \"d\", or \"w\"\n")

(** [convert_str_to_epoch_time str] reads in a string of type "XXh",
    "XXd", or "XXw" and converts it to seconds for the indicators' usage*)
let convert_str_to_epoch_time str =
  (* [seq_to_list sequ] converts sequence [sequ] to a list and reverses
     order*)
  let seq_to_backwards_list sequ =
    let acc = [] in
    Seq.fold_left (fun acc x -> x :: acc) acc sequ
  in
  let char_list_str = seq_to_backwards_list (String.to_seq str) in
  (* first element will be "h"/"d"/"w", then we need to reverse the rest
     of the list*)
  match char_list_str with
  | 'h' :: tl ->
      (* cuz there is 3600 seconds in a hour*)
      epoched_reversed_list tl 3600
  | 'd' :: tl ->
      (* cuz there is 86400 seconds in a day*)
      epoched_reversed_list tl 86400
  | 'w' :: tl ->
      (* 604800 sedons in a week*)
      epoched_reversed_list tl 604800
  | _ ->
      raise
        (Bad_command
           "please format your <time>, <period>, <lookback>, and \
            <delay> as \"XXh\"/\"XXd\"/\"XXw\"\n")

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
            second + (60 * minute) + (3600 * hour) + (86400 * (day - 1))
          in
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
  print_fmt "Analysis Module\n"

(** [print_analyze_cmds d] displays the commands in the Wealth screen to
    the user. The d argument is used to keep track of the dataset in
    question *)
and print_analyze_cmds d =
  (* doesn't have a clear screen param b/c is always called after "help"
     or after showing the wealth *)
  print_fmt "COMMANDS\n";
  print_fmt
    "Please format all <period>, <time>, <lookback>, and <delay> \
     arguments in the following manner:\n";
  print_fmt
    "XXh or XXd or XXw, where XX is an integer and h, d, and w signify \
     hours, days, and weeks\n";
  print_fmt
    "[home]                                     : Return to home\n";
  print_fmt
    "[open <file>]                              : Open <file> to \
     analyze \n";
  print_fmt
    "[sma <period> <num_periods> <time>]        : Displays the simple \
     mean average\n";
  print_fmt
    "                                             looking back from \
     <time>. <num_periods> is an integer. <time> is how long ago from \
     today you would like begin calculating sma.\n";
  print_fmt
    "[ema <period> <num_periods> <time>]        : Displays the \
     exponential moving average\n";
  print_fmt
    "                                             looking back from \
     <time>. <num_periods> is an integer. <time> is how long ago from \
     today you would like begin calculating ema.\n";
  print_fmt
    "[stoch <lookback> <time>]                  : Displays the \
     stochastic oscillator, \n";
  print_fmt
    "                                             looking back \
     <lookback> seconds from <time>. <time> is how long ago from this \
     moment you would like to begin calculating stoch.\n";
  print_fmt
    "[adx <period> <time>]                      : Displays the average \
     directional index, \n";
  print_fmt
    "                                             of data within \
     [time-period, time]. <time> is how long ago from this moment you \
     would like to begin calculating adx. \n";
  print_fmt
    "[macd <period> <time>]                     : Displays the moving \
     average convergence divergence. \n";
  print_fmt
    "                                             of data within \
     [time-period, time]. <time> is how long ago from this moment you \
     would like to begin calculating adx.\n";
  print_fmt
    "[poi <delay> <period> <change>]            : Displays a list of \
     data points in <delay> seconds ago in <period>. \n";
  print_fmt
    "                                             whose change from \
     previous data point is greater than <change>\n";

  print_fmt
    "[help]                                     : Displays these \
     commands again \n"

(** [recieve_analyze_cmds un] is a REPL that reads user's commands
    (specified in [print_analyze_cmds]) and redirects user to function
    that carries out that command*)
and recieve_analyze_cmds d =
  let d = from_csv parsing_fcn1 "ETH_1min_sample.csv" in
  print_string "> ";
  match
    List.filter
      (fun s -> s <> " ")
      (Stringext.full_split (read_line ()) ' ')
  with
  | exception End_of_file -> ()
  | [ "0" ] | [ "home" ] | [ "q" ] | [ "Q" ] | [ "quit" ] | [ "Quit" ]
    ->
      ()
  | [ "open"; filename ] ->
      let new_data = from_csv parsing_fcn1 filename in
      print_endline @@ "Loaded: " ^ filename;
      recieve_analyze_cmds new_data
  | [ "sma"; period; num_periods; time ] -> (
      try
        let avg =
          sma d
            (convert_str_to_epoch_time period)
            (int_of_string num_periods)
            ( (Unix.time () |> int_of_float)
            - convert_str_to_epoch_time time )
        in
        avg |> string_of_float |> print_endline;
        recieve_analyze_cmds d
      with Bad_command s ->
        print_fmt s;
        recieve_analyze_cmds d )
  | [ "ema"; period; num_periods; time ] -> (
      try
        let avg =
          ema d
            (convert_str_to_epoch_time period)
            (* smoothing constant is 2 by default in technical analysis*)
            (int_of_string num_periods)
            ( (Unix.time () |> int_of_float)
            - convert_str_to_epoch_time time )
            ~smoothing:2.
        in
        avg |> string_of_float |> print_endline;
        recieve_analyze_cmds d
      with Bad_command s ->
        print_fmt s;
        recieve_analyze_cmds d )
  | [ "stoch"; lookback; time ] -> (
      try
        stoch d
          (convert_str_to_epoch_time lookback)
          ( (Unix.time () |> int_of_float)
          - convert_str_to_epoch_time time )
        |> string_of_float |> print_endline;
        recieve_analyze_cmds d
      with Bad_command s ->
        print_fmt s;
        recieve_analyze_cmds d )
  | [ "adx"; period; time ] -> (
      try
        adx d
          (convert_str_to_epoch_time period)
          ( (Unix.time () |> int_of_float)
          - convert_str_to_epoch_time time )
        |> string_of_float |> print_endline;
        recieve_analyze_cmds d
      with Bad_command s ->
        print_fmt s;
        recieve_analyze_cmds d )
  | [ "macd"; period; time ] -> (
      try
        macd d
          (convert_str_to_epoch_time period)
          ( (Unix.time () |> int_of_float)
          - convert_str_to_epoch_time time )
        |> string_of_float |> print_endline;
        recieve_analyze_cmds d
      with Bad_command s ->
        print_fmt s;
        recieve_analyze_cmds d )
  | [ "poi"; delay; period; change ] -> (
      try
        points_of_interest d
          (convert_str_to_epoch_time delay)
          (convert_str_to_epoch_time period)
          (float_of_string change)
        |> List.iter (fun dp ->
               string_of_data_point dp |> print_endline);
        recieve_analyze_cmds d
      with Bad_command s ->
        print_fmt s;
        recieve_analyze_cmds d )
  | [ "help" ] | [ "Help" ] ->
      print_analyze_cmds ();
      recieve_analyze_cmds d
  (* a debugging command that won't be officially documented*)
  | [ "print"; "data" ] ->
      print_data d;
      recieve_analyze_cmds d
  | _ ->
      print_fmt
        "I could not understand your choice of command. Please try \
         again, or type [help]\n";
      recieve_analyze_cmds d
