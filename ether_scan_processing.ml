
(** *)
let convert_cur_price str = float_of_string str

let format_date month day year = 
    let string_month = 
    if month = 0 then "January" else if month = 1 then "Feburary" else if 
        month = 2 then "March" else if month = 3 then "April" else if month = 4
            then "May" else if month = 5 then "June" else if month = 6 then "July"
            else if month = 7 then "August" else if month = 8 then "September"
            else if month = 9 then "October" else if month = 10 then "November"
            else if month = 11 then "December" else "invalid month" in
    let string_day = 
        if day mod 10 = 1 && day / 10 != 1 then "st" else if day mod 10 = 2 && 
            day / 10 != 1 then "nd" else if day mod 10 = 3 && day / 10 != 1 
            then "rd" else "th" in
        string_month ^ " " ^ string_of_int day ^ string_day ^ ", " ^ string_of_int year

let format_time hour minute second =
    let string_hour = if hour < 10 then "0" ^ string_of_int hour else 
        string_of_int hour in 
    let string_min = if minute < 10 then "0" ^ string_of_int minute else 
        string_of_int minute in
    let string_sec = if second < 10 then "0" ^ string_of_int second else 
        string_of_int second in 
    string_hour ^ ":" ^ string_min ^ ":" ^ string_sec
(** *)
let convert_time_stamp str = 
    let record = Unix.gmtime (float_of_string str) in
    match record with
    |{tm_sec = sec ; tm_min = min ; tm_hour = hour ; tm_mday = day ; tm_mon = 
    month ; tm_year = year ; tm_wday = weekday ; tm_yday = dayofyear ; 
    tm_isdst = isdaylightsaving} -> (format_time hour min sec) ^ " on " ^
    (format_date month day (year + 1900))
    
(**
let get_price_time un = 
    let pair = ether_scan_query.get_cur_price un in
    match pair with 
    |(price , time) -> (convert_cur_price price, convert_time_stamp time)
    | _ -> (0.0 , "error")
    

(** [formatted_str_price_time ()] returns a cleanly formatted string of
    the form "Current Price: <price>\nAt Time: <time stamp>" (TBD), for
    printing to GUI*)
let formatted_str_price_time un = 
    let pair = ether_scan_query.get_cur_price un in
    match pair with
    | (price , time) -> "Current Price: $" ^ price ^ "\nAt Time" ^ (convert_time_stamp time);
    | _ -> "error"
*)