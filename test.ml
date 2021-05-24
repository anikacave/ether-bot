open OUnit2
open Ether_scan_processing
open Ascii_graph

let format_date_test name str month day year : test =
  name >:: fun _ -> assert_equal str (format_date month day year)

let pp_string s = "\"" ^ s ^ "\""

let format_date_tests =
  [
    format_date_test "0 1 2021 is january 1st 2021" "January 1st 2021" 0
      1 2021;
    format_date_test "4 17 2003 is may 17 2003" "May 17th 2003" 4 17
      2003;
    format_date_test "8 31 1971 is september 31 1971"
      "September 31st 1971" 8 31 1971;
  ]

let format_time_test name str hr min sec : test =
  name >:: fun _ -> assert_equal str (format_time hr min sec)

let format_time_tests =
  [
    format_time_test "1 24 57 is 01:24:57" "01:24:57" 1 24 57;
    format_time_test "7 5 9 is 07:05:09" "07:05:09" 7 5 9;
    format_time_test "9 59 3 is 09:59:03" "09:59:03" 9 59 3;
    format_time_test "23 2 45 is 23:02:45" "23:02:45" 23 2 45;
    format_time_test "12 12 12 is 12:12:12" "12:12:12" 12 12 12;
    format_time_test "17 38 56 is 17:38:56" "17:38:56" 17 38 56;
  ]

let convert_time_stamp_test name str epoch : test =
  name >:: fun _ -> assert_equal str (convert_time_stamp epoch)

let convert_time_stamp_tests =
  [
    convert_time_stamp_test "18000 is 00:00:00 on January 1st 1970"
      "00:00:00 on January 1st 1970" "18000";
    convert_time_stamp_test "928049732 is 03:35:32 on May 30th 1999"
      "03:35:32 on May 30th 1999" "928049732";
    convert_time_stamp_test "1621557079 is 08:31:19 on May 20th 2021"
      "20:31:19 on May 20th 2021" "1621557079";
    convert_time_stamp_test
      "1000000000 is 09:46:40 on September 8th 2001"
      "21:46:40 on September 8th 2001" "1000000000";
  ]

(* Indicators.ml tests TODO: move this to another file and change the
   make test file to run it*)
open Indicators

(* a function that parses ETH_1min_sample.csv into a dataset*)
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
            second + (60 * minute) + (3600 * hour) + (86400 * (day - 1))
          in
          let epochjan12021 = 1609459200 in
          Some (epochjan12021 + epoch, price)

let data =
  from_csv one_min_sample_parsing "ETH_1min_sample.csv" |> rep_ok

let index_of_test name data target expected : test =
  name >:: fun _ ->
  assert_equal expected (index_of data target) ~printer:string_of_int

let index_of_tests =
  [
    index_of_test
      "First timestamp in dataset. Jan 1.\n    expecting ind 0" data
      1609459260 0;
    index_of_test
      "Given target between first and second data\n\
      \    expecting to round down to 0" data 1609459261 0;
    index_of_test
      "Given target between first and second data\n\
      \    expecting to round down to 0" data 1609459319 0;
    index_of_test "Get index 1. Exact time" data 1609459320 1;
    index_of_test "Get index 1. Rounded down" data 1609459321 1;
    index_of_test "Get index 1. Rounded down" data 1609459379 1;
    index_of_test "Get index 2. Exact epoch" data 1609459380 2;
    index_of_test "Get mid index. Exact epoch" data 1609804800 5696;
    index_of_test "Get mid index. Round down" data 1609804801 5696;
    index_of_test "Big number rounds down to latest value" data
      696942069420 19990;
    (* length of entire dataset is 19991*)
    index_of_test "Small number rounds up to earliest value" data 1 0;
  ]

let indicator_tests = List.flatten [ index_of_tests ]

let data =
  from_csv one_min_sample_parsing "test_ether_data.csv" |> rep_ok

let suite =
  "test suite for final project"
  >::: List.flatten
         [
           format_date_tests;
           format_date_tests;
           convert_time_stamp_tests;
           indicator_tests;
         ]

let _ = run_test_tt_main suite
