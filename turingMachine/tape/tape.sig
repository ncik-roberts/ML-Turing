signature TAPE =
sig
  (* Add exactly 1 symbol to the tape alphabet: the blank symbol (NONE) *)
  type 'a tapeSymbol = 'a option
  type direction

  type 'a tape
  val currentSymbol : 'a tape -> 'a tapeSymbol
  (* Write tapeSymbol on current position of tape, then move in direction *)
  val makeMove : 'a tape -> 'a tapeSymbol -> direction -> 'a tape

  (* Create a new tape, drawing only from the input alphabet *)
  val init : 'a list -> 'a tape
  val toList : 'a tape -> 'a tapeSymbol list
end
