open OUnit2
open Ether_scan_processing

let format_date_test name str month day year : test =
  name >:: fun _ -> assert_equal str (format_date month day year)

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

let suite =
  "test suite for final project"
  >::: List.flatten
         [
           format_date_tests;
           format_date_tests;
           convert_time_stamp_tests;
         ]

let _ = run_test_tt_main suite
