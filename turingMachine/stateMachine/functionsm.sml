functor FunctionSM (OPTIONS : sig
                              type action
                              type direction
                            end) : STATE_MACHINE =
struct
  open OPTIONS

  type direction = direction
  type 'a tapeSymbol = 'a option
  datatype 'a sm = Halt of action
               | SM of 'a state
  withtype 'a state = 'a tapeSymbol -> ('a tapeSymbol * direction * 'a sm)

  type 'a move = 'a tapeSymbol * direction * 'a sm

  exception Halted
  (* Strip away datatype constructor and return transition function,
   * if possible *)
  fun nextMove (Halt _) = raise Halted
    | nextMove (SM f) = f

  (* Allows us to make subroutines *)
  fun subroutine sm onHalt =
    let
      fun subroutine' (Halt action) x = onHalt action x
        | subroutine' (SM q) x =
          case q x of (sym, dir, act) => (sym, dir, SM (subroutine' act))
    in
      subroutine' sm
    end
end

