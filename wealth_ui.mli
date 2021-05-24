(** a module for main to display the wealth feature *)

(** [print_show_wealth erase_screen] Displays the user's Ether wealth.
    The parameters are printed from the CSV. All calculations are done
    in [recieve_wealth_commands]*)
val print_show_wealth : bool -> unit

(** [print_wealth_cmds un] Just displays the commands in the Wealth
    screen to the user*)
val print_wealth_cmds : unit -> unit

(** [recieve_wealth_cmds un] is a REPL that reads user's commands
    (specified in [print_wealth_cmds]) and redirects user to function
    that carries out that command*)
val recieve_wealth_cmds : unit -> unit

