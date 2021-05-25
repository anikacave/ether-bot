(** Module for directing the user to useful information on the internet
    regarding investing and cryptocurrency *)

open Unix

(** Prints the general format to show information *)
val print_show_info : bool -> unit

(** Prints the information options to the user *)
val print_info_cmds : unit -> unit

(** Matches the commands to the appropriate feedback or response from
    the information module *)
val recieve_info_cmds : unit -> unit
