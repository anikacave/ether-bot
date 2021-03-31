open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson
open Yojson.Basic.Util
    
(** [get_cur_price ()] is the pair (a, b) where a is the string
    describing the current Ethereum price in USD, and b is the string of
    an integer representing the current Epoch time. Errors: TBD *)
let get_cur_price () =   
    let body = 
        Client.get (Uri.of_string @@ "http://api.etherscan.io/api?"
        ^ "module=stats&action=ethprice"
        ^ "&apikey=5XHR17DKY791V7FSJ8DBEYYZMQGRQWV8NK") 
        >>= fun (resp, body) ->
        body |> Cohttp_lwt.Body.to_string >|= fun body ->   
        body
    in
    let b = Lwt_main.run body in
    let lst = List.nth (b |> Yojson.Basic.from_string |> to_assoc ) 2
    |> snd 
    |> to_assoc in
    (List.nth lst 2 |> snd |> to_string, List.nth lst 3 |> snd |> to_string)
    
(* Exception esp_exception t *)
