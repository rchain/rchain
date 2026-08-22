import Rchain.Sort
import Rchain.Ty

/-!
# Law 3 — capture-avoiding de Bruijn substitution

`sort(subst t) = subst(sort t)`: canonicalization commutes with substitution. The Scala oracle is
`Substitute.scala` + `Env.scala` (de Bruijn levels); the Rust realization is `rholang::substitute`
(total on `Closed`). The *minimal* substitution that the type-system fundamentals need lives in
`Rchain.Ty` (`subst`/`substExpr`); the **deep** capture-avoiding substitution is Coq's Autosubst
obligation (`AGENTS.md`), stated here as an opaque operation with the law it must satisfy.
-/

namespace Rchain

/-- Deep capture-avoiding de Bruijn substitution: replace every free occurrence of a level by its
    image under `σ`, shifting bound levels through binders. This is the operation `Substitute.scala`
    realizes; it is stated (not defined) here — Coq owns the definition. -/
axiom substPar (σ : Var → Par) : Par → Par

/-- Law 3: substitution commutes with canonicalization (`sort(subst t) = subst(sort t)`). -/
axiom sort_subst (σ : Var → Par) (t : Par) : sortPar (substPar σ t) = substPar σ (sortPar t)

/-- Substitution preserves closedness (no free variables introduced). -/
axiom subst_closed (σ : Var → Par) (t : Par) : Closed t → Closed (substPar σ t)

end Rchain
