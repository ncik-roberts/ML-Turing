signature STATE_MACHINE =
sig
  (* What to do when the TM has halted *)
  type action
  type 'a tapeSymbol

  (* Alpha represents the tape symbol, beta represents the direction *)
  type ('a, 'b) state
  datatype ('a, 'b) sm = Halt of action | SM of ('a, 'b) state
  type ('a, 'b) move = 'a tapeSymbol * 'b * ('a, 'b) sm

  (* Exception raised when nextMove is called on a halted sm *)
  exception Halted

  (* Determine the next move (incl. symbol to write, direction of tape head, and
   * next state) based on current configuration *)
  val nextMove : ('a, 'b) sm -> 'a tapeSymbol -> ('a, 'b) move

  (* Make subroutines *)
  val subroutine : ('a, 'b) sm -> (action * 'a tapeSymbol -> ('a, 'b) move)
                     -> 'a tapeSymbol -> ('a, 'b) move
end
