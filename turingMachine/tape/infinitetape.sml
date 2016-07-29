(* Only works with LRS *)
functor InfiniteTape(datatype direction = Left | Right | Stay) : TAPE =
struct
  type direction = direction
  type 'a tapeSymbol = 'a option

  (* Both left and right tapes are stacks of symbols closest
   * to the current symbol. If a tape = L * current * R, then
   * the in-order tape looks like: (rev L) @ (current :: R)
   *)
  type 'a tape = 'a tapeSymbol list * 'a tapeSymbol * 'a tapeSymbol list
  val currentSymbol : 'a tape -> 'a tapeSymbol = #2

  (* Infinite tape, so if we try to go left from the leftmost position,
   * stay there. For right, extend tape. *)
  fun makeMove (L, _, R) symbol Stay = (L, symbol, R)
    (* Loop back *)
    | makeMove ([], _, R) symbol Left  = ([], symbol, R)
      (* Extend tape if necessary *)
    | makeMove (L, _, []) symbol Right = (symbol :: L, NONE, [])
      (* These next two cases discard unneeded blanks at either end *)
    | makeMove (l :: ls, _, []) NONE Left = (ls, l, [])
    | makeMove ([], _, r :: rs) NONE Right = ([], r, rs)
      (* These next two cases actually move left and right *)
    | makeMove (l :: ls, _, R) symbol Left = (ls, l, symbol :: R)
    | makeMove (L, _, r :: rs) symbol Right = (symbol :: L, r, rs)

  fun init [] = ([], NONE, [])
    | init (symbol :: symbols) = ([], SOME symbol, map SOME symbols)

  fun toList (L, current, R) =  List.revAppend (L, current :: R)
end
