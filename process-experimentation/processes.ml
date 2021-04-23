open Unix

let main =
  match fork () with
  | 0 -> print_endline "This is the child process"
  | _ ->
      (* wait (); *)
      print_endline "This is the parent"
