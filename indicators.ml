(** this file calculates indicator values from raw price data *)

(* a list of tuples of epoch time and USD price from csv_data_bot
   Requires: the time is ordered from newest to oldest *)

(* AF: array of time & price pairs RI: all elements are sorted in
   chronological order. No duplicate times *)
type dataset = (int * float) array

let empty_data = Array.make 1 (-1, -1.)

type data_point = {
  price_change : float;
  sma : float;
  stoch : float;
  adx : float;
  macd : float;
}

type op =
  | Low
  | High
  | Mean
  | Sum

(* checks that the dataset is in chronological order with no dupes*)
let rep_ok d : dataset =
  for i = 0 to Array.length d - 2 do
    if fst d.(i) > fst d.(i + 1) then
      failwith "dataset rep invariant violated in indicators.ml"
    else ()
  done;
  (* print_endline "rep ok"; *)
  d

let from_csv parsing_fcn file_name =
  (* TODO consider optimizing from O(2n) to O(n)*)
  let input_stream = open_in file_name in
  let rec scan acc =
    match
      try Some (input_line input_stream) with End_of_file -> None
    with
    | None ->
        Stdlib.close_in input_stream;
        acc
    | Some h -> (
        match parsing_fcn h with
        | None -> acc |> scan
        | Some x -> x :: acc |> scan)
  in
  scan [] |> List.rev |> Array.of_list

(* constructs a dataset from a list of tuples *)
let from_tuple_list (lst : (int * float) list) : dataset =
  let length = List.length lst in
  Array.init length (List.nth lst)

(* binary search index of the target in the dataset. If doesn't exist,
   the lower index from where it would have been*)
let index_of d target =
  let rec helper low high =
    let m = (low + high) / 2 in
    if low <= high then
      if fst d.(m) < target then helper (m + 1) high
      else if fst d.(m) > target then helper low (m - 1)
      else if fst d.(m) = target then m
      else failwith "Impossible???"
    else m
  in
  let low = 0 in
  let high = Array.length d - 1 in
  helper low high

(* returns a subset of the dataset from [trim dataset begin end] is a
   dataset including datapoints between begin and end INCLUSIVE begin
   and end should be in epoch time *)
let rec trim (d : dataset) start finish : dataset =
  (* print_endline ("start is: " ^ (string_of_int start)); print_endline
     ("finish is: " ^ (string_of_int finish)); print_endline "finding
     indices..."; *)
  let ind1 = index_of d start in
  let ind2 = index_of d finish in
  (* ind1 |> string_of_int |> print_endline; ind2 |> string_of_int |>
     print_endline; *)
  let length = ind2 - ind1 in
  (* print_endline ("length is: " ^ (string_of_int length));
     print_endline ("length of d is: " ^ (string_of_int (Array.length
     d))); print_endline ("ind2 is: " ^ (string_of_int ind2)); *)
  (*pog? TODO fix this duct tape *)
  let arr =
    Array.init length (fun i ->
        d.(ind1 + i)
        (* try d.(ind2 + i) with Invalid_argument _ -> (0, 0.) *))
  in
  (* print_endline "trimming2"; print_endline (Array.length arr |>
     string_of_int); *)
  arr |> rep_ok
(* returns a list containing the average price within each period the
   length of the list should be num_intervals. Earlier averages are at
   the head *)

(** returns the desired operation op (a set of potential operations one
    can do on a dataset including the highest price, lowest price, mean
    price, and sum of the prices) on the datase *)
let analyze d op =
  let d = Array.map snd d in
  match op with
  (*don't hard code this but Float.infinity doesn't work*)
  | Low -> Array.fold_left min 9876543210. d
  | High -> Array.fold_left max 0. d
  | Mean ->
      if Array.length d = 0 then 0.
      else Array.fold_left ( +. ) 0. d /. float_of_int (Array.length d)
  | Sum -> Array.fold_left ( +. ) 0. d

let high d start finish : float =
  let trimmed = trim d start finish in
  analyze trimmed High

let low d start finish : float =
  let trimmed = trim d start finish in
  analyze trimmed Low

let mean d start finish : float =
  let trimmed = trim d start finish in
  analyze trimmed Mean

let rec avgs_in_period_list d period time =
  if Array.length d = 0 then []
  else if fst d.(Array.length d - 1) > time then []
  else
    let trimmed = trim d (time - period) time in
    let recurse = avgs_in_period_list d period (time - period) in
    if Array.length trimmed = 0 then recurse
    else analyze trimmed Mean :: recurse

let print_data d =
  let f x =
    "Time: "
    ^ (fst x |> string_of_int)
    ^ " Price: "
    ^ (snd x |> string_of_float)
    |> print_endline
  in
  Array.iter f d

let rec sma d period num_intervals time =
  let trimmed_data = trim d (time - (period * num_intervals)) time in
  let averages = avgs_in_period_list trimmed_data period time in
  (* list of averages*)
  if float_of_int (List.length averages) = 0. then 0.
  else
    List.fold_left ( +. ) 0. averages
    /. float_of_int (List.length averages)

(* calculates the ema given the trimmed dataset period in seconds ie
   86400 is 1 day num_periods how many periods to look back smoothing
   constant*)

let rec ema d period num_periods time ?(smoothing = 2.) =
  if num_periods <= 0 || time <= fst d.(0) then 0.
  else
    let trimmed = trim d (time - period) time in
    (* print_endline @@ "not 0: " ^ string_of_float ((float_of_int
       num_periods +. 1.)); *)
    let k = smoothing /. (float_of_int num_periods +. 1.) in
    (analyze trimmed Mean *. k)
    +. ema d period (num_periods - 1) (time - period) ~smoothing:2.
       *. (1. -. k)

(* [stoch d lookback time] is the stochastic oscillator looking back
   [lookback] seconds from time [time] Note: this method performs no
   smoothing Requires: the given dataset has enough information to
   lookback the desired amount and contains the time *)
let stoch (d : dataset) lookback time =
  let trimmed = trim d (time - lookback) time in
  if Array.length trimmed = 0 then 0.
  else if Array.length trimmed <= 1 then 0.
  else
    let closing = snd trimmed.(Array.length trimmed - 1) in
    let low = analyze trimmed Low in
    let high = analyze trimmed High in
    (closing -. low) /. (high -. low) *. 100.

let sma_accessible d =
  let latest =
    d (* latest data point in d*) |> Array.length
    |> ( + ) (-1) (* commutativity problem*)
    |> Array.get d |> fst
  in
  sma d 86400 14 latest
(* 14 day daily sma*)

let ema_accessible d =
  let latest =
    d (* latest data point in d*) |> Array.length
    |> ( + ) (-1) (* commutativity problem*)
    |> Array.get d |> fst
  in
  ema d 86400 14 latest ~smoothing:2.
(* 14 day daily ema*)

let stoch_accessible d =
  let latest =
    d (* latest data point in d*) |> Array.length
    |> ( + ) (-1) (* commutativity problem*)
    |> Array.get d |> fst
  in
  stoch d 86400 latest
(* stoch of the last day*)

let adx d period time =
  (* pos directional movement of two periods looking back from time*)
  let pos_dm period time =
    let curr_high = analyze (trim d (time - period) time) High
    and prev_high =
      analyze (trim d (time - (2 * period)) (time - period)) High
    in
    let up_move = curr_high -. prev_high in
    if up_move > 0. then up_move else 0.
  and neg_dm period time =
    let curr_low = analyze (trim d (time - period) time) Low
    and prev_low =
      analyze (trim d (time - (2 * period)) (time - period)) Low
    in
    let up_move = curr_low -. prev_low in
    if up_move > 0. then up_move else 0.
    (* TODO implement fully*)
  in
  let atr period time =
    let trimmed = trim d (time - period) time in
    let high = analyze trimmed High
    and low = analyze (trim d (time - period) time) Low
    and closing =
      if Array.length trimmed = 0 then Float.neg_infinity
      else Array.length trimmed - 1 |> Array.get trimmed |> snd
    in
    assert (high <> Float.infinity);
    assert (low <> Float.infinity);
    assert (closing <> Float.infinity);
    high -. low
    |> max (Float.abs (high -. closing))
    |> max (Float.abs (low -. closing))
  in
  assert (pos_dm period time <> Float.infinity);
  assert (neg_dm period time <> Float.infinity);
  (* assert ((pos_dm period time -. neg_dm period time) <> 0.); *)
  (* assert ((atr period time) <> 0.); *)
  atr period time |> ( /. ) (pos_dm period time -. neg_dm period time)

let macd d period time =
  (*TODO: Calculate a nine-period EMA of of this result?*)
  ema d period 12 time ~smoothing:2.
  -. ema d period 26 time ~smoothing:2.

let adx_accessible d =
  let latest =
    d (* latest data point in d*) |> Array.length
    |> ( + ) (-1) |> Array.get d |> fst
  in
  adx d 86400 latest
(* adx of the last day*)

let macd_accessible d =
  let latest =
    d (* latest data point in d*) |> Array.length
    |> ( + ) (-1) |> Array.get d |> fst
  in
  macd d 86400 latest
(* macd with period of 1 day compares 26 day with 12 day*)

let generate_datapoints (data : dataset) delay period : data_point array
    =
  (* array size safety. If empty, empty data_point*)
  if Array.length data = 0 then
    Array.make 0
      {
        price_change = 0.;
        sma = 0.;
        (* sma of the last 48 hours*)
        stoch = 0.;
        (* stoch of the last day*)
        adx = 0.;
        macd = 0.;
      }
  else
    let latest = fst data.(Array.length data - 1)
    and earliest = fst data.(0) in
    let length =
      2 * (latest - earliest) / period
      (* how many data points to create*)
    in
    let f i =
      (* index to data_point*)
      let from = earliest |> ( + ) (i * period / 2) in
      let new_price =
        from + delay (* price after a delay*)
        |> index_of data |> Array.get data |> snd
      and old_price = from |> index_of data |> Array.get data |> snd in
      {
        price_change = (new_price -. old_price) /. old_price;
        sma = sma data 3600 48 from;
        (* sma of the last 48 hours*)
        stoch = stoch data 86400 from;
        (* stoch of the last day*)
        (* adx = 0.; macd = 0.; *)
        adx =
          (if adx data 86400 from <> Float.infinity then
           adx data 86400 from
          else failwith "calculated infinity");
        macd = macd data 86400 from;
      }
    in
    Array.init length f

(** filters the points of interest where the price change is greater
    than the specified change*)
let points_of_interest data delay period change =
  (* generate_datapoints data delay period |> Array.length |>
     string_of_int |> print_endline; *)
  generate_datapoints data delay period
  |> Array.to_list
  |> List.filter
     @@
     if change > 0. then fun x -> x.price_change > change
     else fun x -> x.price_change < change

let string_of_data_point (data_point : data_point) =
  "Price change: "
  ^ string_of_float data_point.price_change
  ^ " SMA: "
  ^ string_of_float data_point.sma
  ^ " Stoch: "
  ^ string_of_float data_point.stoch
  ^ " ADX: "
  ^ string_of_float data_point.adx
  ^ " MACD: "
  ^ string_of_float data_point.macd
