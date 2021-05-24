open Indicators 
open Analyze

let sample_fcn str =
  let vals = String.split_on_char ',' str in
  try
    Some
      ( List.nth vals 0 |> int_of_string,
        List.nth vals 1 |> float_of_string )
  with Failure _ -> None
let suggestion () = 
  (* the suggestion is based on our bot data that is currently being generated *)
  let d = from_csv sample_fcn "ether_data.csv" in 
  "buy"