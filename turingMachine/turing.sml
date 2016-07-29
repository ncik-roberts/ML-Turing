functor Turing (
    structure T : TAPE
    structure SM : STATE_MACHINE
    where type 'a tapeSymbol = 'a T.tapeSymbol
    sharing type T.direction = SM.direction) : TURING_MACHINE =
struct
  structure T = T
  structure SM = SM

  (* Simulate 1 step on the stateMachine and tape *)
  fun step stateMachine tape =
    let
      val symbol = T.currentSymbol tape
      val (newSymbol, direction, stateMachine') =
        SM.nextMove stateMachine symbol
      val tape' = T.makeMove tape newSymbol direction
    in
      (stateMachine', tape')
    end

  (* Simulate until halt *)
  fun simulate' (SM.Halt action, tape) = (action, tape)
    | simulate' (sm, tape) = simulate' (step sm tape)
  fun simulate stateMachine input =
    case simulate' (stateMachine, T.init input)
      of (action, tape) => (action, T.toList tape)
end
