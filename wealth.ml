type filename = string

type amount_ether = float

type price = float

type worth = float

type spent = float

exception InvalidEtherAmount of string

exception InsufficientEtherInOwn of string

let ether_own un = 0.0

let ether_own_add amt = 
  if (amt > 0.) then () else raise (InvalidEtherAmount "cannot buy a non-positive amount of ether")
(** [ether_own_sub amt_ether] updates the log file to reflect recent
    Ether sell*)
let ether_own_sub amt = 
  if (amt > 0.) then () else raise (InvalidEtherAmount "cannot sell a non-positive amount of ether")

let ether_spent un = 0.0

let ether_spent_add amt cur_price =
  if (amt > 0.) then () else raise (InvalidEtherAmount "cannot buy a non-positive amount of ether")

(** [ether_worth_sub amt_ether cur_price] updates the log file to
    reflect recent Ether sells. Updates the spent of ether in stores*)
let ether_spent_sub amt cur_price = 
  if (amt > 0.) then () else raise (InvalidEtherAmount "cannot sell a non-positive amount of ether")

(** [ether_worth cur_price] is the worth of [ether_own ()], which is the
    amount of ether in stores times the current_price*)
let ether_worth (cur_price : price) : worth = 0.0
