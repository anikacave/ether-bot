type filename = string

type amount_ether = float

type price = float

type worth = float

type spent = float

type revenue = float

exception InvalidEtherAmount of string

exception InsufficientEtherInOwn of string

let ref_own = ref 0.

let ref_worth = ref 0.

let ref_spent = ref 0.

let ref_rev = ref 0.

let fname = "ether_wealth.csv"

(* all the csv stuff that user doesn't need to know about *)

(** [initialize_own_row] is "own, 0.0"*)
let initialize_own_row = "own, 0.0\n"

(** [initialize_worth_row] is "worth, 0.0"*)
let initialize_worth_row = "worth, 0.0\n"

(** [initialize_spent_row] is "spent, 0.0"*)
let initialize_spent_row = "spent, 0.0\n"

(** [initialize_revenue_row] is "revenue, 0.0"*)
let initialize_revenue_row = "revenue, 0.0\n"

(** [create_csv file] creates csv @ given filename and initializes own,
    worth, spent, and revenue *)
let create_csv (file : filename) =
  let buff = open_out file in
  (*from anika's csv_line:*)
  output_string buff initialize_own_row;
  output_string buff initialize_worth_row;
  output_string buff initialize_spent_row;
  output_string buff initialize_revenue_row;
  (*back to create_csv*)
  Stdlib.close_out buff

(** [update_csv amt_ether cur_price bought] updates [ether_wealth.csv]
    by pasting in the values of our refs into the csv*)
let update_csv un =
  if Sys.file_exists fname then () else create_csv fname

(** [scan_csv_for value fname] returns the float value of
    [own]/[worth]/[spent]/[revenue] (whichever string specified) located
    in [fname]*)
let scan_csv_for value fname =
  let input_stream = open_in fname in
  let rec scan un =
    match
      try Some (input_line input_stream) with End_of_file -> None
    with
    | None ->
        Stdlib.close_in input_stream;
        raise
          (Failure
             ( "could not initialize " ^ value ^ ", not found in "
             ^ fname ))
    | Some h ->
        let vals = String.split_on_char ',' h in
        if List.nth vals 0 = value then (
          (* if you find a row w/ first entry the value you're looking
             for *)
          let close un = Stdlib.close_in input_stream in
          close ();
          float_of_string (List.nth vals 1)
          (* then return the float beside it*) )
        else scan ()
  in
  scan ()

(** [initialize_wealth un] creates the csv log if it does not exist and
    updates the value refs *)
let initialize_wealth un =
  if Sys.file_exists fname then () else create_csv fname;
  ref_own := scan_csv_for "own" fname;
  ref_worth := scan_csv_for "worth" fname;
  ref_spent := scan_csv_for "spent" fname;
  ref_rev := scan_csv_for "revenue" fname

(* done with csv stuff *)

let restart_wealth un =
  create_csv fname;
  initialize_wealth ()

(** [update_values amt_ether cur_price bought] updates the refs [own],
    [worth], [spent], and [revenue], and returns a tuple of the new
    values*)
let update_values amt_ether cur_price bought =
  if bought then (
    ref_own := !ref_own +. amt_ether;
    ref_worth := !ref_own *. cur_price;
    ref_spent := !ref_spent +. (amt_ether *. cur_price);
    (* ref_rev stays the same*)
    (!ref_own, !ref_worth, !ref_spent, !ref_rev) )
  else (
    ref_own := !ref_own -. amt_ether;
    ref_worth := !ref_own *. cur_price;
    ref_rev := !ref_rev +. (amt_ether *. cur_price);
    (* ref_spent stays the same*)
    (!ref_own, !ref_worth, !ref_spent, !ref_rev) )

let wealth_bought amt_ether cur_price =
  let to_return = update_values amt_ether cur_price true in
  update_csv ();
  to_return

let wealth_sold amt_ether cur_price =
  let to_return = update_values amt_ether cur_price false in
  update_csv ();
  to_return

let ether_own un = !ref_own

let ether_own_add amt =
  if amt > 0. then amt
  else
    raise
      (InvalidEtherAmount "cannot buy a non-positive amount of ether")

(** [ether_own_sub amt_ether] updates the log file to reflect recent
    Ether sell*)
let ether_own_sub amt =
  if amt > 0. then amt
  else
    raise
      (InvalidEtherAmount "cannot sell a non-positive amount of ether")

let ether_spent un = !ref_spent

let ether_spent_add amt cur_price =
  if amt > 0. then amt
  else
    raise
      (InvalidEtherAmount "cannot buy a non-positive amount of ether")

(** [ether_worth_sub amt_ether cur_price] updates the log file to
    reflect recent Ether sells. Updates the spent of ether in stores*)
let ether_spent_sub amt cur_price =
  if amt > 0. then amt
  else
    raise
      (InvalidEtherAmount "cannot sell a non-positive amount of ether")

(** [ether_worth cur_price] is the worth of [ether_own ()], which is the
    amount of ether in stores times the current_price*)
let ether_worth (cur_price : price) : worth = !ref_worth

let ether_liquid_rev un = !ref_rev

let ether_liquid_rev_add amt cur_price =
  if amt > 0. then amt
  else
    raise
      (InvalidEtherAmount
         "failure in ether_liquid_rev: try to add negative amount \
          (liquid_rev should never decrease)")
