functor FunctionSM (OPTIONS : sig
                              type action
                            end) : STATE_MACHINE =
struct
  open OPTIONS

  type 'a tapeSymbol = 'a option
  datatype ('a, 'b) sm = Halt of action
               | SM of ('a, 'b) state
  withtype ('a, 'b) move = 'a tapeSymbol * 'b * ('a, 'b) sm
  and      ('a, 'b) state = 'a tapeSymbol -> ('a, 'b) move


  exception Halted
  (* Strip away datatype constructor and return transition function,
   * if possible *)
  fun nextMove (Halt _) = raise Halted
    | nextMove (SM f) = f

  (* Allows us to make subroutines *)
  fun subroutine sm onHalt =
    let
      fun subroutine' (Halt action) x = onHalt (action, x)
        | subroutine' (SM q) x =
          case q x of (sym, dir, act) => (sym, dir, SM (subroutine' act))
    in
      subroutine' sm
    end
end

