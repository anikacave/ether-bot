open Wealth
open Ether_scan_processing
open Wealth_bot

(* ANSITerminal formatting*)
let print_fmt str = ANSITerminal.(print_string [ magenta ] str)

let rec print_show_wealth erase_screen =
  if erase_screen then (
    ANSITerminal.(
      erase Screen;
      set_cursor 1 1);
    print_fmt "Welcome to Wealth!\n";
    print_fmt
      "Here you can buy and sell Ether in a safe, fake money \
       environment.\n";
    print_fmt "Ask our bot what she thinks you should do --\n";
    print_fmt
      "maybe her prowess will convince you to spend some money IRL!\n";
    print_fmt
      "(best part is, what you buy and sell today will update with new \
       valuation tomorrow!)\n";
    print_fmt "WEALTH\n" )
  else ();

  print_fmt ("You own " ^ string_of_float (ether_own ()) ^ " Ether\n");
  print_fmt
    ( "Worth: $"
    ^ string_of_float (ether_worth (just_cur_price ()))
    ^ "\n" );
  print_fmt ("Spent: $" ^ string_of_float (ether_spent ()) ^ "\n");
  print_fmt
    ( "Made (Liquid Revenue): $"
    ^ string_of_float (ether_liquid_rev ())
    ^ "\n" )

and print_wealth_cmds un =
  (* doesn't have a clear screen param b/c is always called after "help"
     or after showing the wealth *)
  print_fmt "COMMANDS\n";
  print_fmt
    "[1 <ff.ff>] - [buy <ff.ff>]              : Buy <ff.ff> Ether\n";
  print_fmt
    "[2 <ff.ff>] - [sell <ff.ff>]             : Sell <ff.ff> Ether\n";
  print_fmt
    "[3] - [restart]                          : Set \
     own/worth/spent/rev to 0\n";
  print_fmt
    "[4] - [home]                             : Return to home\n";
  print_fmt
    "[5] - [ask bot]                          : See if bot recommends \
     buying or selling\n";
  print_fmt
    "[help]                                   : Displays commands\n"

and recieve_wealth_cmds un =
  print_string "> ";
  match
    List.filter
      (fun s -> s <> " ")
      (Stringext.full_split (read_line ()) ' ')
  with
  | exception End_of_file -> ()
  | [ "5" ] | [ "ask"; "bot" ] ->
      print_fmt ("you should: " ^ suggestion () ^ "\n");
      recieve_wealth_cmds ()
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
  | [ "4" ] | [ "home" ] | [ "quit" ] | [ "Quit" ] -> ()
  | [ "help" ] | [ "Help" ] ->
      print_fmt "[Help]:\n";
      print_wealth_cmds ();
      recieve_wealth_cmds ()
  | _ ->
      print_fmt
        "I could not understand your choice of command. Please try \
         again, or type [help]\n";
      recieve_wealth_cmds ()
