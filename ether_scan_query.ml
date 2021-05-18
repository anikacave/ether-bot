open Lwt
open Cohttp
open Cohttp_lwt_unix
open Yojson
open Yojson.Basic.Util

exception Query_Failed of string

type extreme =
  | High
  | Low

(** [get_cur_price ()] is the pair (a, b) where a is the string
    describing the current Ethereum price in USD, and b is the string of
    an integer representing the current Epoch time. Errors: TBD *)
let get_cur_price () =
  try
    let body =
      Uri.of_string @@ "http://api.etherscan.io/api?"
      ^ "module=stats&action=ethprice"
      ^ "&apikey=5XHR17DKY791V7FSJ8DBEYYZMQGRQWV8NK"
      |> Client.get
      >>= fun (resp, body) ->
      if resp |> Response.status != Response.(`OK) then
        raise
        @@ Query_Failed
             ( "HTTP request failed with status code "
             ^ ( resp |> Response.status |> Code.code_of_status
               |> string_of_int ) )
      else body |> Cohttp_lwt.Body.to_string >|= fun body -> body
    in
    let json = Lwt_main.run body |> Yojson.Basic.from_string in

    (* print_endline (json |> to_assoc |> List.assoc "status" |>
       to_string ); *)
    if
      json |> to_assoc |> List.assoc "status" |> to_string
      |> int_of_string == 0
    then
      raise
      @@ Query_Failed
           ( "Failed to retrieve from etherscan.io: recieved status 0. "
           ^ "Check your internet connection or contact the developers \
              for additional API keys." )
    else
      (* Extract info from the json. Consult the json structure at
         https://api.etherscan.io/api?module=stats&action=ethprice&apikey=YourApiKeyToken*)
      let lst = List.nth (json |> to_assoc) 2 |> snd |> to_assoc in
      ( List.nth lst 2 |> snd |> to_string,
        List.nth lst 3 |> snd |> to_string )
  with
  | Failure a when a = "resolution failed: name resolution failed" ->
    raise
      (Query_Failed
         "Something went wrong. Check your internet connection.")

type hl_data = {
  high : float;
  low : float;
}

let hl_data_of_json j =
  {
    high = j |> member "high" |> to_float;
    low = j |> member "low" |> to_float;
  }

(* Helper function for formatting the date. If invalid date error is
   caught in query function *)
let date_format date =
  let m_d_y = String.split_on_char '/' date in
  List.nth m_d_y 2 ^ "-" ^ List.nth m_d_y 0 ^ "-" ^ List.nth m_d_y 1

(* Makes a query to return the extreme high or low price of an ether ETF
   on a given date. Throws query failed if the request fails or an
   invalid date is given. If the request fails with a valid date it
   throws a different error than if the query failed due to internet or
   connection issues and is dealt with under the error messages. The
   extreme parameter is either High or Low depending if the high or low
   value from that day is being searched for. The parameter date is in
   the form MM/DD/YYYY *)
let get_ext (ext : extreme) (date : string) =
  try
    let date_formatted = date_format date in
    let body =
      Uri.of_string
      @@ "http://api.marketstack.com/v1/eod?access_key=fab625d5da3643c521ac84bb1e834cd0&symbols=ETHR.XTSE&date_from="
      ^ date_formatted ^ "&date_to=" ^ date_formatted
      |> Client.get
      >>= fun (resp, body) ->
      if resp |> Response.status != Response.(`OK) then
        raise
        @@ Query_Failed
             ( "HTTP request failed with status code "
             ^ ( resp |> Response.status |> Code.code_of_status
               |> string_of_int ) )
      else body |> Cohttp_lwt.Body.to_string >|= fun body -> body
    in
    let json = Lwt_main.run body |> Yojson.Basic.from_string in

    (* print_endline (json |> to_assoc |> List.assoc "status" |>
       to_string ); *)
    let lst = List.nth (json |> to_assoc) 1 |> snd |> to_list in
    let vals = hl_data_of_json (List.nth lst 0) in
    match ext with High -> vals.high | Low -> vals.low
  with
  | Failure a when a = "resolution failed: name resolution failed" ->
      raise
        (Query_Failed
           "Something went wrong. Check your internet connection.")
  | Failure s -> raise (Query_Failed "invalid date/weekend")

(* Gets the historical high price at given date in format MM/DD/YYYY *)
let get_historical_high date = get_ext High date

(* Gets the historical low price at given date in format MM/DD/YYYY *)
let get_historical_low date = get_ext Low date
