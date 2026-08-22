# Substitution and matching

Two operations make COMM actually *do* something: **substitution** (Law 3) moves the sent data into
the receive body, and **spatial matching** (Law 5) decides whether a message fits a pattern.

## Substitution (Law 3)

A comm substitutes the sent data for the receive's bound variable. That substitution must be
**capture-avoiding** and respect de Bruijn levels: renaming bound variables so a free variable of the
substituted term is never captured by a binder it lands under.

The minimal substitution the type system needs is in
[`spec/Rchain/Ty.lean`](../../../spec/Rchain/Ty.lean) (`subst`, `substExpr`, `substListExpr`), and it is
proven **sort-preserving**:

```lean
theorem subst_classify (σ : Subst) (p : Par) : classify (subst σ p) = classify p
theorem subst_preserves_sort (σ : Subst) {t : Par} {s : PSort} (h : HasSort t s) : HasSort (subst σ t) s
```

The **deep** capture-avoiding substitution is stated (not defined) in
[`spec/Rchain/Subst.lean`](../../../spec/Rchain/Subst.lean), with the law it must satisfy:

```lean
axiom substPar : (Var → Par) → Par → Par
axiom sort_subst (σ : Var → Par) (t : Par) : sortPar (substPar σ t) = substPar σ (sortPar t)
axiom subst_closed (σ : Var → Par) (t : Par) : Closed t → Closed (substPar σ t)
```

`sort_subst` is the exact Law-3 statement: **canonicalization commutes with substitution**
(`sort(subst t) = subst(sort t)`). `subst_closed` is the companion guarantee that substitution does not
introduce free variables.

The Coq track owns the *definition* of capture-avoiding de Bruijn substitution (Autosubst-style):
`substPar` and `subst_commutes_sort` in [`spec/coq/Laws.v`](../../../spec/coq/Laws.v). The K executable
form is `free.k` (the free-variable function) together with the substitution module it references.

## Spatial matching (Law 5)

Matching walks a pattern and a message together. The two invariants are:

```lean
def BindsAtMostOnce (p : Par) : Prop :=
  ∀ n m : Nat, freeVarOf p n → freeVarOf p m → n = m

axiom spatialMatches : Par → Par → Prop
axiom spatialMatches_decidable (target pattern : Par) : Decidable (spatialMatches target pattern)
```

`BindsAtMostOnce` is the "a free variable is bound at most once" invariant (`addedVars.distinct` in
the Scala oracle, carried in Rust as the `freeCount` of `BindPattern`). `spatialMatches_decidable` is
the totality guarantee: the matcher *decides* match-or-no-match, with no silent partiality.

The K executable form splits matching into several rules: `matching-function.k` (the general arity
matcher), `specific-matching-rules.k` (variable binding and substitution), `exact-matching-function.k`
(the "look through the looking glass once" exact match for patterns within patterns), and
`matching-with-par.k` (matching a parallel composition greedily).

## How they combine in a COMM

A comm is, in one line:

```
send chan!(x) | receive for(pat ← chan){ body }   ⟶   body[ x / pat ]
```

1. **Match** `pat` against `x` (Law 5): if they don't fit, no comm.
2. **Bind** the pattern's free variables to the matched sub-terms.
3. **Substitute** (Law 3) those bindings into `body`, capture-avoiding.

The result is `body` with the message woven in — the single primitive that the whole language is built
on.

> Next: the guarantee that none of this can go wrong — [Closedness and the Calculus of
> Constructions](closedness-coc.md).
