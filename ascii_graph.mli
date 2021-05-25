(** Graphs Ether data using ascii astricks *)

(** type filename to keep track of file inputs *)
type filename = string

(** if the dataset is too small then throw small data set *)
exception Small_Data_Set

(** raise if there is an error parsing the dataset *)
exception CSV_Parse_Failure

(** Makes a graph from [file] and prints to terminal *)
val make_graph : filename -> unit

(** Makes a graph from [file] and returns the ascii graph in string form *)
val string_graph : filename -> string
