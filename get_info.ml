open Unix

let open_url url =
  Unix.system ("open " ^ url);
  ()

let what_is_crypto () =
  open_url
    "https://www.nerdwallet.com/article/investing/cryptocurrency-7-things-to-know"

let elon_tweets () =
  open_url
    "https://twitter.com/elonmusk?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor"

let messari_graph () = open_url "https://messari.io/asset/ethereum"

let crypto_news_yahoo () =
  open_url "https://finance.yahoo.com/topic/crypto/"

let print_fmt str = ANSITerminal.(print_string [ magenta ] str)

let rec print_show_info erase_screen =
  if erase_screen then (
    ANSITerminal.(erase Screen);
    ANSITerminal.set_cursor 1 1 )
  else ();
  print_fmt "Info Module\n"

and print_info_cmds () =
  print_fmt "ASK ME...\n";
  print_fmt "[1] What is cryptocurrency?\n";
  print_fmt "[2] What is Elon up to today?\n";
  print_fmt "[3] View Ether live graph\n";
  print_fmt "[4] New crypto news\n";
  print_fmt
    "[help]                                     : Displays these \
     commands again \n"

and recieve_info_cmds () =
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
  | [ "1" ] ->
      what_is_crypto ();
      recieve_info_cmds ()
  | [ "2" ] ->
      elon_tweets ();
      recieve_info_cmds ()
  | [ "3" ] ->
      messari_graph ();
      recieve_info_cmds ()
  | [ "4" ] ->
      crypto_news_yahoo ();
      print_info_cmds ()
  | [ "help" ] | [ "Help" ] ->
      print_info_cmds ();
      recieve_info_cmds ()
  | _ ->
      print_fmt
        "I could not understand your choice of command. Please try \
         again, or type [help]\n";
      recieve_info_cmds ()
