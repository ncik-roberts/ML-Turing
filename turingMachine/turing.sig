signature TURING_MACHINE =
sig
  structure T : TAPE
  structure SM : STATE_MACHINE
  where type 'a tapeSymbol = 'a T.tapeSymbol

  type 'a sm = ('a, T.direction) SM.sm

  (* Simulate a single step on the turing machine *)
  val step : 'a sm -> 'a T.tape -> 'a sm * 'a T.tape

  (* Simulate the turing machine until it halts. *)
  val simulate : 'a sm -> 'a list -> SM.action * 'a T.tapeSymbol list
end
