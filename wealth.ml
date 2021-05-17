type filename = string

type amount_ether = float

type price = float

type worth = float

type spent = float

type liquid_rev = float

exception InvalidEtherAmount of string

exception InsufficientEtherInOwn of string

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
    based on whether the user bought or sold [amt_ether]*)
let update_csv amt_ether cur_price bought =
  if Sys.file_exists fname then () else create_csv fname

(* done with csv stuff *)

let wealth_bought amt_ether cur_price =
  update_csv amt_ether cur_price true;
  (0.0, 0.0, 0.0, 0.0)

let wealth_sold amt_ether cur_price =
  update_csv amt_ether cur_price false;
  (0.0, 0.0, 0.0, 0.0)

let ether_own un = 0.0

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

let ether_spent un = 0.0

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
let ether_worth (cur_price : price) : worth = 0.0

let ether_liquid_rev un = 0.0

let ether_liquid_rev_add amt cur_price =
  if amt > 0. then amt
  else
    raise
      (InvalidEtherAmount
         "failure in ether_liquid_rev: try to add negative amount \
          (liquid_rev should never decrease)")
