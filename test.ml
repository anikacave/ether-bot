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

let round num =
  let make_int = int_of_float (num *. 100000.) in
  float_of_int make_int /. 100000.

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

let trim_test name data start finish expected : test =
  name >:: fun _ ->
  assert_equal (from_tuple_list expected) (trim data start finish)

let trim_tests =
  [
    trim_test "trim from 0 to 0" data 0 0 [];
    trim_test "trim from start to start" data 1609459260 1609459260 [];
    trim_test "trim from start to one minute later" data 1609459260
      1609459320
      [ (1609459260, 743.77) ];
    trim_test "trim from start to two minutes later" data 1609459260
      1609459380
      [ (1609459260, 743.77); (1609459320, 742.33) ];
    trim_test "trim from start to three minutes later" data 1609459260
      1609459440
      [
        (1609459260, 743.77); (1609459320, 742.33); (1609459380, 741.978);
      ];
    trim_test "trim from start to ten minutes later" data 1609459260
      1609459860
      [
        (1609459260, 743.77);
        (1609459320, 742.33);
        (1609459380, 741.978);
        (1609459440, 742.39);
        (1609459500, 742.19);
        (1609459560, 742.48);
        (1609459620, 741.479);
        (1609459680, 741.35);
        (1609459740, 740.63);
        (1609459800, 740.7);
      ];
  ]

let high_test name data start finish expected : test =
  name >:: fun _ ->
  assert_equal expected
    (high data start finish)
    ~printer:string_of_float

let high_tests =
  [
    high_test "high of empty" (from_tuple_list []) 0 0 0.;
    high_test "high from start to start" data 1609459260 1609459260 0.;
    high_test "high from start to one minute later" data 1609459260
      1609459320 743.77;
    high_test "high from start to two minutes later" data 1609459260
      1609459380 743.77;
    high_test "high from start to three minutes later" data 1609459260
      1609459440 743.77;
    high_test "high from start to ten minutes later" data 1609459260
      1609459860 743.77;
  ]

let low_test name data start finish expected : test =
  name >:: fun _ ->
  assert_equal expected (low data start finish) ~printer:string_of_float

let low_tests =
  [
    low_test "low of empty" (from_tuple_list []) 0 0 9876543210.;
    low_test "low of empty" (from_tuple_list []) 1609459260 1609459260
      9876543210.;
    low_test "low from start to one minute later" data 1609459260
      1609459320 743.77;
    low_test "low from start to two minutes later" data 1609459260
      1609459380 742.33;
    low_test "low from start to three minutes later" data 1609459260
      1609459440 741.978;
    low_test "low from start to ten minutes later" data 1609459260
      1609459860 740.63;
  ]

let mean_test name data start finish expected : test =
  name >:: fun _ ->
  assert_equal (round expected)
    (round (mean data start finish))
    ~printer:string_of_float

let mean_tests =
  [
    mean_test "mean of empty" (from_tuple_list []) 0 0 0.;
    mean_test "mean from start to start" data 1609459260 1609459260 0.;
    mean_test "mean from start to one minute later" data 1609459260
      1609459320 743.77;
    mean_test "mean from start to two minutes later" data 1609459260
      1609459380 743.05;
    mean_test "mean from start to three minutes later" data 1609459260
      1609459440 742.692666667;
    mean_test "mean from start to ten minutes later" data 1609459260
      1609459860 741.9297;
  ]

let sma_test name data period num time expected : test =
  name >:: fun _ ->
  assert_equal expected
    (sma data period num time)
    ~printer:string_of_float

let sma_tests =
  [
    sma_test "sma from 0 to 0" data 0 0 0 0.;
    sma_test "sma from start to start" data 60 0 1609459260 0.;
    sma_test "sma from start to one minute later 1 period of 1 minute"
      data 60 1 1609459320 0.;
    sma_test "sma from start to one minute later 2 periods of 1 minute"
      data 120 1 1609459320 0.;
    sma_test "sma from start to two minutes later 2 periods of 1 minute"
      data 60 2 1609459380 743.77;
    sma_test "sma from start to two minutes later 3 periods of 1 minute"
      data 120 1 1609459380 743.77;
    (* sma_test "sma from start to three minutes later 2 periods of 1
       minute" data 60 3 1609459440 743.05; sma_test "sma from start to
       three minutes later 3 periods of 1 minute" data 120 2 1609459440
       743.05;*)
  ]

let ema_test name data period num time expected : test =
  name >:: fun _ ->
  assert_equal expected
    (ema data period num time ~smoothing:2.)
    ~printer:string_of_float

let ema_tests =
  [
    ema_test "ema from 0 to 0" data 0 0 0 0.;
    ema_test "ema from start to start" data 60 0 1609459260 0.;
    ema_test
      "ema from\n   start to one minute later 1 period of 1 minute" data
      60 1 1609459320 0.;
    ema_test
      "ema from start to one minute later 2 periods of 1\n   minute"
      data 120 1 1609459320 0.;
    ema_test
      "ema from start to two\n   minutes later 2 periods of 1 minute"
      data 60 2 1609459380 743.77;
    ema_test "ema from start to two minutes later 3 periods of 1 minute"
      data 120 1 1609459380 743.77;
    ema_test
      "ema from start to ten minutes\n   later 20 periods of 1 minute"
      data 60 10 1609459860 741.93;
  ]

let stoch_test name data lookback time expected : test =
  name >:: fun _ ->
  assert_equal (round expected)
    (round (stoch data lookback time))
    ~printer:string_of_float

let stoch_tests =
  [
    stoch_test "stoch from 0 to 0" data 0 0 0.;
    stoch_test "stoch from start to start" data 0 1609459260 0.;
    stoch_test "stoch from start to one minute later 1 lookback" data 60
      1609459320 0.;
    stoch_test "stoch from start to two minutes later 1 lookback" data
      120 1609459380 0.;
    stoch_test "stoch from start to three minutes later 1 lookback" data
      180 1609459440 0.;
    stoch_test "stoch from start to ten minutes later 1 lookback" data
      600 1609459860 2.22929936306;
    stoch_test "stoch from start to twenty minutes later 1 lookback"
      data 1200 1609460460 62.4203821656;
  ]

let indicator_tests =
  List.flatten
    [
      index_of_tests;
      trim_tests;
      high_tests;
      low_tests;
      mean_tests;
      sma_tests;
      stoch_tests;
    ]

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
