(** Lets user query indicators SMA, EMA, MACD, and ADX themselves*)

open Indicators

(** [print_show_analyze erase_screen] The Analyze intro.*)
val print_show_analyze : bool -> unit

(** [print_analyze_cmds] displays the commands in the Analysis screen to
    the user.*)
val print_analyze_cmds : unit -> unit

(** [recieve_analyze_cmds un] is a REPL that reads user's commands
    (specified in [print_analyze_cmds]) and redirects user to function
    that carries out that command*)
val recieve_analyze_cmds : dataset -> unit

(** [epoched_reversed_list lst multiplier] creates a string in the rigth
    order out of [lst] and multiplies its integer equivalent by
    [multiplier]*)
val epoched_reversed_list : char list -> int -> int

(** [convert_str_to_epoch_time str] reads in a string of type "XXh",
    "XXd", or "XXw" and converts it to seconds for the indicators' usage*)
val convert_str_to_epoch_time : String.t -> int
