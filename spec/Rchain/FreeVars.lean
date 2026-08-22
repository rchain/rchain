import Rchain.Ty

/-!
# Law 6 — no globally free variables

A program is closed: no free (unbound) de Bruijn level occurs in it. The Scala oracle is
`free.k`/`program-restrictions.k` + `HasLocallyFree.scala`; the Rust realization is
`models::types::Closed` (a refinement newtype). `Closed` is already defined, made decidable, and
proven preserved by composition/`≡`/`⟶` in `Rchain.Ty`; this module adds the *semantic* free-variable
predicate and ties it to `Closed`.
-/

namespace Rchain

/-- The free-variable predicate: `freeVarOf p n` holds when the de Bruijn level `n` occurs free in
    `p`. Stated here (the definition is Coq's α-equivalence obligation); `Closed` is the structural,
    decidable reading. -/
axiom freeVarOf : Par → Nat → Prop

/-- Law 6: a process is closed exactly when it has no free variables. -/
axiom closed_iff_no_freeVars (p : Par) : Closed p ↔ ∀ n, ¬ freeVarOf p n

end Rchain
