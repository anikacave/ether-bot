open Indicators 
open Analyze

let suggestion un = 
  (* the suggestion is based on our bot data that is currently being generated *)
  let d = from_csv sample_fcn "ether_data.csv" in 
  "buy"