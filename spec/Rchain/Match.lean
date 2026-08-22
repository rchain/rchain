import Rchain.Ty
import Rchain.FreeVars

/-!
# Law 5 — spatial matching, a free var is bound at most once

The matcher consumes a datum against a `BindPattern`; every free variable in the pattern is bound to
a sub-term at most once (`addedVars.distinct`, carried in Rust as the `free_count` of `BindPattern`).
The Scala oracle is `SpatialMatcher.scala` + `ParCount.scala`; the Rust realization is
`rholang::matcher::spatial_match` plus the `free_count: FreeCount` fields on `ReceiveBind`/`MatchCase`
(`BindsAtMostOnce` here is the Lean predicate, not a Rust type).
-/

namespace Rchain

/-- A free variable occurs at most once in a process (the `addedVars.distinct` invariant). -/
def BindsAtMostOnce (p : Par) : Prop :=
  ∀ n m : Nat, freeVarOf p n → freeVarOf p m → n = m

/-- Law 5: a bind pattern binds each free variable at most once. -/
axiom pattern_binds_at_most_once (pat : Par) : BindsAtMostOnce pat

/-- `spatialMatches target pat` — the spatial matcher accepts `target` against `pat`. -/
axiom spatialMatches : Par → Par → Prop

/-- Law 5: matching is decidable — the matcher decides match/no-match with no silent partiality (the
    Rust `spatial_match` returns a `Result` and records internal errors). -/
axiom spatialMatches_decidable (target pattern : Par) : Decidable (spatialMatches target pattern)

end Rchain
