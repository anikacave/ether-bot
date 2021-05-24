(** the little bot that sits in our wealth module, and recommends when you should buy and sell. 
Its only visible function is [suggestion], which spits out either "buy" or "sell", based on the 4 indicators
SMA, EMA, STOCH, and MACD, using data from the csv our bot generates*)

(** [suggestion] is "buy" if bot decides you should buy in this moment, and "sell" if bot decides you should sell*)
val suggestion : unit -> string