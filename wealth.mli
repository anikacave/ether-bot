(** The type filename *)
type filename = string

(** An amount of Ether *)
type amount_ether = float

(** A current price of Ether *)
type price = float

(** The worth of a given [amount_ether] based on the current [price]*)
type worth = float

(** How much the user has spent based on the historical [price] of their
    individual Ether purchases*)
type spent = float

(** When Ether specified is greater than 99.99, or 0 *)
exception InvalidEtherAmount of string

(** When user tries to sell more Ether than they posess *)
exception InsufficientEtherInOwn of string

(** [ether_own ()] gives the current amount of Ether, which is between 0
    and 99.99, owned by the user *)
val ether_own : unit -> amount_ether

(** [ether_own_add amt_ether] updates the log file to reflect recent
    Ether purchase*)
val ether_own_add : amount_ether -> unit

(** [ether_own_sub amt_ether] updates the log file to reflect recent
    Ether sell*)
val ether_own_sub : amount_ether -> unit

(** [ether_spent ()] gives the current money spent on Ether owned by the
    user, based on historical [prices] of individual Ether in store*)
val ether_spent : unit -> spent

(** [ether_spent_add amt_ether cur_price] updates the log file to
    reflect recent Ether purchases. Updates the spent of ether in log*)
val ether_spent_add : amount_ether -> price -> unit

(** [ether_spent_sub amt_ether cur_price] updates the log file to
    reflect recent Ether sells. Updates the spent of ether in log*)
val ether_spent_sub : amount_ether -> price -> unit

(** [ether_worth cur_price] is the worth of [ether_own ()], which is the
    amount of ether in stores times the current_price*)
val ether_worth : price -> worth
