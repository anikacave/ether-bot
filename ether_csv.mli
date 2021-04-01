(** The type filename *)
type filename = string

(** Exception thrown when searching for a price at time*)
exception TimestampNotFound

(** Exception thrown when a file has an invalid extension/format ex) a
    file is named file instead of file.csv *)
exception InvalidFileExtensionFormat

(** [create_csv file] creates csv @ given filename (will be from current
    data availible and should be only one line long to include the most
    current information) *)
val create_csv : filename -> filename

(** [from_csv time file] is the price of the ethereum at given [time].
    If no such entry exists, raises TimeStampNotFound *)
val from_csv : float -> filename -> string

(** [update_csv file] appends the current data to the a specified csv
    file. Writes to a seperate file than the original one specified to
    avoid deleting all data from a csv file. returns the name of the new
    file that was written to. Raises InvalidFileExtensionFormat if the
    file extension is illegal*)
val safe_update_csv : filename -> filename
