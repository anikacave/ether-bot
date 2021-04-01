
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson
open Yojson.Basic.Util
    

exception Query_Failed of string

(** [get_cur_price ()] is the pair (a, b) where a is the string
    describing the current Ethereum price in USD, and b is the string of
    an integer representing the current Epoch time. Errors: TBD *)
let get_cur_price () =   
    let body = 
        Uri.of_string @@ "http://api.etherscan.io/api?"
        ^ "module=stats&action=ethprice"
        ^ "&apikey=5XHR17DKY791V7FSJ8DBEYYZMQGRQWV8NK"
        |> Client.get 
        >>= fun (resp, body) ->
        if resp |> Response.status != Response.(`OK) 
            then raise @@ Query_Failed ("HTTP request failed with status code " 
                ^ (resp |> Response.status |> Code.code_of_status |> string_of_int) )
            else (body |> Cohttp_lwt.Body.to_string >|= fun body -> body)
    in
    let json = Lwt_main.run body |> Yojson.Basic.from_string in

    (* print_endline (json |> to_assoc |> List.assoc "status" |> to_string ); *)
    if (json |> to_assoc |> List.assoc "status" |> to_string |> int_of_string == 0) then
        raise @@ Query_Failed ("Failed to retrieve from etherscan.io: recieved status 0. "
        ^ "Check your internet connection or contact the developers for additional API keys." 
        )
    else 
        (* Extract info from the json. Consult the json structure at 
        https://api.etherscan.io/api?module=stats&action=ethprice&apikey=YourApiKeyToken*)
        let lst = List.nth (json |> to_assoc ) 2
        |> snd 
        |> to_assoc in
        (List.nth lst 2 |> snd |> to_string , List.nth lst 3 |> snd |> to_string)

