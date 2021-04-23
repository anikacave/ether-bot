open Unix

let main =
  match fork () with
  | 0 ->
      print_endline "This is the child process";
      execv "text_opener.byte" [||]
  | _ ->
      wait ();
      print_endline "this is the parent"
