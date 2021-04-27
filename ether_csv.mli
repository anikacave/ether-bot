(** The type filename *)
type filename = string

(* Format of the csv file, true if user-friendly and false if
   bot-friendly *)
type readable_format = bool

type timestamp = float

type price = float

(** Exception thrown when searching for a price at time*)
exception TimestampNotFound

(* Exception when an invalid csv file is thrown *)
exception InvalidCSVFile

(** Exception thrown when a file has an invalid extension/format ex) a
    file is named file instead of file.csv *)
exception InvalidFileExtensionFormat

(** [create_csv file] creates csv @ given filename (will be from current
    data availible and should be only one line long to include the most
    current information) *)
val create_csv : filename -> readable_format -> filename

(** [from_csv time file] is the price of the ethereum at given [time].
    If no such entry exists, raises TimeStampNotFound *)
val from_csv : float -> filename -> string

(** [update_csv file] appends the current data to the a specified csv
    file. Writes to a seperate file than the original one specified to
    avoid deleting all data from a csv file. returns the name of the new
    file that was written to. Raises InvalidFileExtensionFormat if the
    file extension is illegal*)
val safe_update_csv : filename -> readable_format -> filename

(* Finds the highest price in the past 24 hours. Searches through the
   CSV data. If data from the past 24 hours is not included in the csv
   file, throws the TimestampNotFound exception. Throws invalid CSV file
   if the CSV is not readable *)
val high_today : filename -> timestamp * price

(* Finds the lowest price in the past 24 hours. Searches through the CSV
   data. If data from the past 24 hours is not included in the csv file,
   throws the TimestampNotFound exception. Throws invalid CSV file if
   the CSV is not readabl *)
val low_today : filename -> timestamp * price

val get_current_epoch_time : unit -> timestamp
