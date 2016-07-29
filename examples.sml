structure TM_Creator =
struct
  datatype action = Accept | Reject

  structure TM = Turing(
    structure T = BiinfiniteTape(open LRS)
    structure SM = FunctionSM(
      type direction = LRS.direction
      type action = action))

  (* Have access to SM constructor and subroutine without an annoying prefix *)
  open TM.SM
  (* Have access to Left/Right/Stay without an annoying prefix *)
  open LRS

  datatype symbol = One | Zero

  (* Moves the tape to the right one starting from current position *)
  (* Halts with head on rightmost nonblank *)
  fun qRightZero NONE        = (SOME Zero, Stay, Halt Accept)
    | qRightZero (SOME One)  = (SOME Zero, Right, SM qRightOne)
    | qRightZero (SOME Zero) = (SOME Zero, Right, SM qRightZero)
  and qRightOne NONE         = (SOME One,  Stay, Halt Accept)
    | qRightOne (SOME One)   = (SOME One,  Right, SM qRightOne)
    | qRightOne (SOME Zero)  = (SOME One,  Right, SM qRightZero)

  (* Moves the tape to the left one starting from current position *)
  (* Halts with head on leftmost nonblank *)
  fun qLeftZero NONE        = (SOME Zero, Stay, Halt Accept)
    | qLeftZero (SOME One)  = (SOME Zero, Left, SM qLeftOne)
    | qLeftZero (SOME Zero) = (SOME Zero, Left, SM qLeftZero)
  and qLeftOne NONE         = (SOME One,  Stay, Halt Accept)
    | qLeftOne (SOME One)   = (SOME One,  Left, SM qLeftOne)
    | qLeftOne (SOME Zero)  = (SOME One,  Left, SM qLeftZero)

  (* Let's separate every pair of ones *)
  fun qStart NONE              = (NONE,      Left,  Halt Accept)
    | qStart (SOME Zero)       = (SOME Zero, Right, SM qStart)
    | qStart (SOME One)        = (SOME One,  Right, SM qJustReadOne)
  and qJustReadOne NONE        = (NONE,      Left,  Halt Accept)
    | qJustReadOne (SOME Zero) = (SOME Zero, Right, SM qStart)
    | qJustReadOne (SOME One)  = (SOME One, Left, SM (qDepair ()))
  and qDepair () =
        let
          fun onHalt _ x = (x, Stay, SM qStart)
        in
          subroutine (SM qLeftZero) onHalt
        end
end

structure TM_Tester =
struct
  open TM_Creator

  val (Accept, [SOME Zero]) = TM.simulate (SM qRightZero) []

  val tape = [One, One]
  val (Accept, [SOME Zero, SOME One, SOME One]) = TM.simulate (SM qRightZero) tape
  val (Accept, [SOME One, SOME One, SOME One]) = TM.simulate (SM qLeftOne) tape

  (* Let's separate the ones in friends! *)
  val depair = SM qStart
  val friends = [One, One, Zero, Zero, One, One, One]
  val result = [SOME One, SOME Zero, SOME One, SOME Zero, SOME Zero, SOME One,
                SOME Zero, SOME One, SOME Zero, SOME One]
  val true = (Accept, result) = TM.simulate depair friends
end
