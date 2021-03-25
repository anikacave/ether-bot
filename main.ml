(* Here we should open Ether_csv, Ether_scan_processing,
   Ether_scan_query *)
exception File_not_found

let print_fmt str =
  ANSITerminal.(print_string [ magenta; on_white ] str)

(** [yn_start ()] prompts user if they mean to enter the bot, calls
    [prompt_cmds ()] if "Y" and quits if "N". Repeats until desired
    input achieved*)
let rec yn_start () =
  print_string "(Y)es/(N)o >";
  match read_line () with
  | exception End_of_file -> ()
  | "N" | "n" | " n" | " N" -> print_endline "Quitting..."
  | "Y" | "y" | " y" | " Y" -> failwith "TODO"
  | _ ->
      print_endline
        "Please respond with \"Y\" for yes, or \"N\" for no (quit)";
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
