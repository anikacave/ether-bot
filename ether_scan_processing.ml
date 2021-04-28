open Ether_scan_query

(** exception*)
exception Invalid_date of string

(** [convert_cur_price ()] is the floating point number equivalent of
    the string input*)
let convert_cur_price str = float_of_string str

(** [format_date()] returns a readable string in the form "Month_name
    day(st/nd/rd/th), year" from three integers month (0-11) day (0-31)
    and year (1970-inf.) *)
let format_date month day year =
  if year < 1970 || year > 2100 then
    raise (Invalid_date "Invalid input year")
  else
    let string_month =
      if month = 0 then "January"
      else if month = 1 then "Feburary"
      else if month = 2 then "March"
      else if month = 3 then "April"
      else if month = 4 then "May"
      else if month = 5 then "June"
      else if month = 6 then "July"
      else if month = 7 then "August"
      else if month = 8 then "September"
      else if month = 9 then "October"
      else if month = 10 then "November"
      else if month = 11 then "December"
      else raise (Invalid_date "Invalid input month")
    in
    let string_day =
      if day > 31 || day < 0 then
        raise (Invalid_date "Invalid input day")
      else if day mod 10 = 1 && day / 10 != 1 then "st"
      else if day mod 10 = 2 && day / 10 != 1 then "nd"
      else if day mod 10 = 3 && day / 10 != 1 then "rd"
      else "th"
    in
    string_month ^ " " ^ string_of_int day ^ string_day ^ " "
    ^ string_of_int year

(** [format_time()] returns a readable string in the form "hh:mm:ss"
    from three integers 0-59 h,m,s *)
let format_time hour minute second =
  let string_hour =
    if hour < 10 then "0" ^ string_of_int hour else string_of_int hour
  in
  let string_min =
    if minute < 10 then "0" ^ string_of_int minute
    else string_of_int minute
  in
  let string_sec =
    if second < 10 then "0" ^ string_of_int second
    else string_of_int second
  in
  string_hour ^ ":" ^ string_min ^ ":" ^ string_sec

(** [convert_time_stamp ()] is the cleanly formatted string of the form
    "hh:mm:ss On month day, year" found from unix timestamp string and
    converted to a readable string *)
let convert_time_stamp str =
  let record = Unix.localtime (float_of_string str) in
  match record with
  | {
   tm_sec = sec;
   tm_min = min;
   tm_hour = hour;
   tm_mday = day;
   tm_mon = month;
   tm_year = year;
   tm_wday = weekday;
   tm_yday = dayofyear;
   tm_isdst = isdaylightsaving;
  } ->
      format_time hour min sec
      ^ " on "
      ^ format_date month day (year + 1900)

(* CLEAN THIS UP LATER*)
let readable_to_unix str =
  let splitcomma = String.split_on_char ',' str in
  let splitspace = String.split_on_char ' ' (List.hd splitcomma) in
  let date = String.split_on_char '-' (List.hd splitspace) in
  let time = String.split_on_char ':' (List.hd (List.tl splitspace)) in
  let year = int_of_string (List.hd date) in
  let month = int_of_string (List.nth date 1) in
  let day = int_of_string (List.nth date 2) in
  let hour = int_of_string (List.hd time) in
  let minute = int_of_string (List.nth time 1) in
  let second = int_of_string (List.nth time 2) in
  let price = float_of_string (List.nth splitcomma 4) in
  let epoch =
    second + (60 * minute) + (3600 * hour) + (86400 * (day - 1))
  in
  let epochjan12021 = 1609477200 in
  (epochjan12021 + epoch, price)

(** [get_price_time ()] is the pair (a, b) where a is the float
    describing the current Ethereum price in USD, and b is the string
    describing the date at which the Ether price was queried, in
    [hh:mm:ss day month year]. Errors: TBD *)
let get_price_time un =
  let pair = get_cur_price un in
  match pair with
  | price, time -> (convert_cur_price price, convert_time_stamp time)

(** [formatted_str_price_time ()] returns a cleanly formatted string of
    the form "Current Price: <price>\nAt Time: <time stamp>" (TBD), for
    printing to UI*)
let formatted_str_price_time un =
  let pair = get_price_time un in
  match pair with
  | price, time ->
      "Current Price: $" ^ string_of_float price ^ "\nAt Time " ^ time

(** [csv_bot_price_time ()] returns a CSV-friendly string of the form
    ["epoch_time, price"]*)
let csv_bot_price_time un =
  let pair = get_cur_price un in
  match pair with price, time -> time ^ ", " ^ price

(** [csv_readable_price_time ()] returns a CSV-friendly string of the
    form ["hh:mm:ss on month day year, price"]*)
let csv_readable_price_time un =
  let pair = get_price_time un in
  match pair with price, time -> time ^ ", " ^ string_of_float price

(** [csv_readable_price_time ()] returns a CSV-friendly string of the
    form ["hh:mm:ss month day year, price"]*)
let csv_readable_price_time un =
  let pair = get_price_time un in
  match pair with
  | price, time ->
      String.(sub time 0 8)
      ^ String.(sub time 11 (length time - 11))
      ^ ", " ^ string_of_float price
