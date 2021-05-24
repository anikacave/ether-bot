open Indicators 
open Analyze

let sample_fcn str =
  let vals = String.split_on_char ',' str in
  try
    Some
      ( List.nth vals 0 |> int_of_string,
        List.nth vals 1 |> float_of_string )
  with Failure _ -> None

let one_min_sample_parsing str =
  let splitcomma = String.split_on_char ',' str in
  if splitcomma = [] then None
  else
    let splitspace = String.split_on_char ' ' (List.hd splitcomma) in
    if splitspace = [] then None
    else
      let date = String.split_on_char '-' (List.hd splitspace) in
      if date = [] then None
      else
        let time = String.split_on_char ':' (List.nth splitspace 1) in
        if time = [] then None
        else
          let day = int_of_string (List.nth date 2) in
          let hour = int_of_string (List.hd time) in
          let minute = int_of_string (List.nth time 1) in
          let second = int_of_string (List.nth time 2) in
          let price = float_of_string (List.nth splitcomma 4) in
          let epoch =
            second + (60 * minute) + (3600 * hour) 
            + (86400 * (day - 1)) in
          let epochjan12021 = 1609459200 in
          Some (epochjan12021 + epoch, price)
  (* rudimentary weighting of three indicators for
  a price prediction*)
let predict sma stoch adx macd = 
  (stoch -. 60.) *. 0.25
  |> (+.) @@ (adx) *. 0.437
  |> (+.) @@ macd *. 0.2
  

let suggestion () = 
  (* the suggestion is based on our bot data that is currently being generated *)


  (*our sample data isn't working rn. Using 1 min file instead*)
  (* let d = from_csv sample_fcn "ether_data.csv" in  *)

  let d = from_csv one_min_sample_parsing "ETH_1min_sample.csv" in 

  let sma = sma_accessible d
  in let stoch = stoch_accessible d 
  in let adx = adx_accessible d
  in let macd = macd_accessible d

  in predict sma stoch adx macd
  |> string_of_float
  |> (^) "Expecting price change of: "
  |> (^) @@ "SMA: " ^ (string_of_float sma)
  |> (^) (string_of_float stoch)
  |> (^) (string_of_float adx)
  |> (^) (string_of_float macd)

  