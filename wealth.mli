(** The type filename *)
type filename = string

(** An amount of Ether *)
type amount_ether = float

(** A current price of Ether *)
type price = float

(** The worth of a given [amount_ether] based on the current [price]*)
type worth = float

(** How much the user has spent based on the historical [price] of their
    individual Ether purchases (is affected only by buys, not sells)*)
type spent = float

(** How much the user has made by selling previously purchased Ether. As
    usual, Net Profit = Revenue - Cost (Spent)*)

type revenue = float

(** When Ether specified is greater than 99.99, or 0 *)
exception InvalidEtherAmount of string

(** When user tries to sell more Ether than they posess *)
exception InsufficientEtherInOwn of string

(** [restart_wealth un] clears the CSV log and references, so user can
    start from clean slate *)
val restart_wealth : unit -> unit

(** [initialize_wealth un] creates the csv log if it does not exist and
    updates the value refs *)
val initialize_wealth : unit -> unit

(** [ether_own ()] gives the current amount of Ether, which is between 0
    and 99.99, owned by the user *)
val ether_own : unit -> amount_ether

(** [ether_spent ()] gives the current money spent on Ether owned by the
    user, based on historical [prices] of individual Ether in store*)
val ether_spent : unit -> spent

(** [ether_worth cur_price] is the worth of [ether_own ()], which is the
    amount of ether in stores times the current_price*)
val ether_worth : price -> worth

(** [ether_liquid_rev ()] gives the net revenue made from seling
    previously purchased Ether, based on historical prices of individual
    Ether + when they were sold*)
val ether_liquid_rev : unit -> revenue

(** [wealth_bought amt cur_price] returns the updated values of [own],
    [worth], [spent] and [liquid_rev] SIDE EFFECT: updates the log file
    [ether_wealth.csv]*)
val wealth_bought :
  amount_ether -> price -> amount_ether * worth * spent * revenue

(** [wealth_sold amt cur_price] returns the updated values of [own],
    [worth], [spent] and [liquid_rev] SIDE EFFECT: updates the log file
    [ether_wealth.csv]*)
val wealth_sold :
  amount_ether -> price -> amount_ether * worth * spent * revenue

(** [ether_own_add amt_ether] updates the log file to reflect recent
    Ether purchase, and returns the new amount of ether owned*)
val ether_own_add : amount_ether -> amount_ether

(** [ether_own_sub amt_ether] updates the log file to reflect recent
    Ether sell, and returns the new amount of ether owned*)
val ether_own_sub : amount_ether -> amount_ether

(** [ether_spent_add amt_ether cur_price] updates the log file to
    reflect recent Ether purchases. Updates the spent of ether in log,
    and returns the new value*)
val ether_spent_add : amount_ether -> price -> spent

(** NOTE: new decision, should not be subtracting spent. Instead add a
    new variable, liquid_rev, that increases each time you make a sale
    [ether_spent_sub amt_ether cur_price] updates the log file to
    reflect recent Ether sells. Updates the spent of ether in log, and
    returns the new value*)
val ether_spent_sub : amount_ether -> price -> spent

(** [ether_liquid_rev_add amt_ether] updates the log file's liquid_rev
    to reflect a recent sale of ether, and returns the new value*)

val ether_liquid_rev_add : amount_ether -> price -> revenue
