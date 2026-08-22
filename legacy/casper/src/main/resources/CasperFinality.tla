---------------------------- MODULE CasperFinality ----------------------------
EXTENDS Integers, FiniteSets

(***************************************************************************)
(* CBC-Casper finality rule, reconstructed from                            *)
(* `block-storage/.../dag/Finalizer.scala` + `MessageMapSyntax.scala`.      *)
(*                                                                         *)
(*   Law 14 — finality requires strictly > 2/3 of bonded stake             *)
(*            (exact integer: 3·stake > 2·total).                          *)
(*   Law 15 — the fringe is an antichain (one message per bonded           *)
(*            validator) and the seen set is monotone.                     *)
(*   Law 16 — seqNum strictly increases along self-justifications;         *)
(*            content addressing.                                          *)
(*                                                                         *)
(* A message (block) is a node in a DAG via `justifications`. The fringe   *)
(* is the latest message per bonded validator. A block is finalized once   *)
(* the stake of validators whose latest message *sees* it is a > 2/3       *)
(* supermajority.                                                          *)
(***************************************************************************)

CONSTANTS
  Validators,   (* the bonded validator set *)
  Blocks        (* the set of all block/message ids *)

VARIABLES
  dag,          (* dag[b] = [sender, seqNum, justifications] for b \in Blocks *)
  latestMsg     (* latestMsg[v] = latest block from validator v (the fringe) *)

vars == << dag, latestMsg >>

(***************************************************************************)
(* Stake                                                                  *)
(***************************************************************************)

CONSTANT Stake
ASSUME StakePositive == \A v \in Validators : Stake[v] > 0

(* Weighted sum of `Stake` over a finite validator set. *)
RECURSIVE SumStake(_)
SumStake(S) ==
  IF S = {} THEN 0
  ELSE LET v == CHOOSE x \in S : TRUE
       IN Stake[v] + SumStake(S \ {v})

TotalStake == SumStake(Validators)

(* Law 14: strictly more than 2/3 of bonded stake (exact integer). *)
IsSuperMajority(stake, total) == 3 * stake > 2 * total

(***************************************************************************)
(* DAG accessors                                                          *)
(***************************************************************************)

Sender(b)         == dag[b].sender
SeqNum(b)         == dag[b].seqNum
Justifications(b) == dag[b].justifications

(***************************************************************************)
(* Well-formedness (Law 16)                                               *)
(***************************************************************************)

JustificationsWellFormed ==
  \A b \in Blocks :
    /\ Justifications(b) \subseteq Blocks
    /\ \A j \in Justifications(b) : Sender(j) \in Validators

SeqNumStrictlyIncreases ==
  \A b \in Blocks :
    \A j \in Justifications(b) :
      Sender(j) = Sender(b) => SeqNum(j) < SeqNum(b)

(***************************************************************************)
(* Seen set: transitive closure of justifications (Law 15)                *)
(***************************************************************************)

RECURSIVE SeenClosure(_, _)
SeenClosure(frontier, acc) ==
  IF frontier = {} THEN acc
  ELSE LET next == UNION { Justifications(x) : x \in frontier }
       IN SeenClosure(next, acc \cup next)

Seen(b) == SeenClosure({b}, {b})

(***************************************************************************)
(* Fringe (Law 15): one latest message per bonded validator, an antichain *)
(***************************************************************************)

FringeWellFormed ==
  /\ \A v \in Validators : latestMsg[v] \in Blocks /\ Sender(latestMsg[v]) = v
  /\ \A v \in Validators :
       \A w \in Validators \ {v} : latestMsg[w] \notin Seen(latestMsg[v])

(* Seen-set monotonicity: each validator's latest message sees all earlier
   fringe messages — the seen set never shrinks. *)
SeenMonotone ==
  \A v \in Validators :
    \A b \in Seen(latestMsg[v]) :
      Seen(b) \subseteq Seen(latestMsg[v])

(***************************************************************************)
(* Finality (Law 14)                                                      *)
(***************************************************************************)

(* The bonded stake whose latest message sees `b`. *)
SupportingStake(b) ==
  SumStake({ v \in Validators : b \in Seen(latestMsg[v]) })

(* A block is finalized when a > 2/3 supermajority of stake sees it. *)
Finalized(b) == IsSuperMajority(SupportingStake(b), TotalStake)

(* Finality is stable: a finalized block is seen by every bonded validator's
   latest message (so no future message can un-see it). *)
FinalizedIsSeenByAll ==
  \A b \in Blocks :
    Finalized(b) => \A v \in Validators : b \in Seen(latestMsg[v])

(***************************************************************************)
(* Fault tolerance                                                         *)
(*                                                                         *)
(* The exact normalized `faultTolerance` value is not recovered from the   *)
(* Scala oracle (it appears only as a monotone assertion in the            *)
(* integration tests). The safety margin of a finalized block is the       *)
(* excess of its supporting stake over the 2/3 threshold:                  *)
(*     margin(b) = 3·SupportingStake(b) - 2·TotalStake      (> 0 iff       *)
(*                                                        Finalized(b)).  *)
(* The integration test asserts fault tolerance is non-increasing as the   *)
(* DAG grows; we state the corresponding monotonicity of the margin.       *)
(***************************************************************************)

FaultToleranceMargin(b) == 3 * SupportingStake(b) - 2 * TotalStake

FaultToleranceNonIncreasing ==
  \A b \in Blocks :
    \A b' \in Seen(b) : FaultToleranceMargin(b) <= FaultToleranceMargin(b')

(***************************************************************************)
(* The safety invariant every reachable state satisfies.                  *)
(***************************************************************************)

Inv ==
  /\ JustificationsWellFormed
  /\ SeqNumStrictlyIncreases
  /\ FringeWellFormed
  /\ SeenMonotone
  /\ FinalizedIsSeenByAll

=============================================================================
