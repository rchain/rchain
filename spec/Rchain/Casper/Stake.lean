/-!
# Law 14 — finality requires > 2/3 bonded stake

A fringe finalizes iff the stake supporting it is a strict supermajority (`> 2/3`). The Scala oracle
is `sdk/consensus/Stake.scala:8`; the Rust realization is `sdk::consensus::is_super_majority` with the
exact integer comparison `3·stake > 2·total` (no floating-point precision loss).
-/

namespace Rchain

/-- A bonded validator. -/
structure Validator where
  id : Nat

/-- A bond: a validator and its (non-negative) stake. -/
structure Bond where
  validator : Validator
  stake : Nat

/-- The supermajority test: `stake` is strictly more than two thirds of `total` — the exact-integer
    spelling `3·stake > 2·total` (Law 14), not the lossy `stake/total > 2/3`. -/
def isSuperMajority (stake total : Nat) : Prop := 3 * stake > 2 * total

/-- Law 14: a supporting stake finalizes iff it is a supermajority of the total bonded stake. -/
axiom finality_iff_supermajority (supporting total : Nat) :
  isSuperMajority supporting total ↔ supporting * 3 > total * 2

end Rchain
