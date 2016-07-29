signature STATE_MACHINE =
sig
  (* What to do when the TM has halted *)
  type action
  (* Direction of the head to move. *)
  type direction

  type 'a state
  type 'a tapeSymbol
  datatype 'a sm = Halt of action | SM of 'a state
  type 'a move = 'a tapeSymbol * direction * 'a sm

  (* Exception raised when nextMove is called on a halted sm *)
  exception Halted

  (* Determine the next move (incl. symbol to write, direction of tape head, and
   * next state) based on current configuration *)
  val nextMove : 'a sm -> 'a tapeSymbol -> 'a move

  (* Make subroutines *)
  val subroutine : 'a sm -> (action -> 'a tapeSymbol -> 'a move) -> 'a tapeSymbol -> 'a move
end
