type filename = string

exception Small_Data_Set

exception CSV_Parse_Failure

val make_graph : filename -> unit

val string_graph : filename -> string
