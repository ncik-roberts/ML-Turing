signature TURING_MACHINE =
sig
  structure T : TAPE
  structure SM : STATE_MACHINE
  where type 'a tapeSymbol = 'a T.tapeSymbol
  sharing type  T.direction =  SM.direction

  (* Simulate a single step on the turing machine *)
  val step : 'a SM.sm -> 'a T.tape -> 'a SM.sm * 'a T.tape

  (* Simulate the turing machine until it halts. *)
  val simulate : 'a SM.sm -> 'a list -> SM.action * 'a T.tapeSymbol list
end
