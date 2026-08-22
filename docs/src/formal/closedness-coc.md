# Closedness and the Calculus of Constructions

The last piece of the formal picture is the guarantee that the interpreter **cannot go wrong** — the
"no silent partiality" invariant. It is built on Law 6 (**closedness**) and on embedding the ρ-calculus
as the base sort of a **Calculus of Constructions** (CoC).

## Closedness (Law 6)

A process is **closed** when it has no free (unbound) variables. `Closed` is defined structurally in
[`spec/Rchain/Ty.lean`](../../../spec/Rchain/Ty.lean) over the flat `Par`:

```lean
def Closed (p : Par) : Prop :=
  closedListSend p.sends = true ∧ … ∧ closedListConnective p.connectives = true
```

It is **decidable** (`closed_decidable`), and — this is the point — it is **preserved** by every
operation the interpreter performs:

```lean
theorem Closed_nil : Closed nilPar
theorem Closed_parMerge_iff (p q : Par) : Closed (parMerge p q) ↔ Closed p ∧ Closed q
theorem strCong_closed {p q : Par} (h : StrCong p q) (hp : Closed p) : Closed q
theorem reduce_closed {p p' : Par} (h : Reduce p p') (hp : Closed p) : Closed p'
```

Closedness is a monoid invariant under `|`, and it is invariant under both `≡` and `⟶`. In other words:
**start from a closed program, and it stays closed, forever.** The semantic reading — `Closed p ↔ no
free variables` — is stated as `closed_iff_no_freeVars` in
[`spec/Rchain/FreeVars.lean`](../../../spec/Rchain/FreeVars.lean).

## The Calculus of Constructions layer

The ρ-calculus is embedded as the **base sort** of a Calculus of Constructions — the dependent type
system that Lean and Coq implement. This is what turns "the interpreter is total" from an aspiration
into a *theorem*. The six fundamentals (from [`spec/TYPE-SYSTEM.md`](../../../spec/TYPE-SYSTEM.md)) are:

| # | Fundamental | Lean theorem |
|---|---|---|
| F1 | sort classification is functional and decidable | `HasSort_functional`, `hasVarSort_decidable` |
| F2 | `≡` is an equivalence and a congruence | `strCong_equivalence` |
| F3 | substitution preserves sort | `subst_preserves_sort` |
| F4 | reduction preserves sort and closedness | `reduce_closed` |
| F5 | canonicalization commutes with typing | `sortList_mem_pred` |
| F6 | totality is compositional | `TotalOn_id`, `TotalOn_comp` |

## Totality and refinement types

**Totality** is the statement that an operation maps closed processes to closed processes:

```lean
def TotalOn (f : Par → Par) : Prop := ∀ p, Closed p → Closed (f p)
```

`TotalOn` is compositional (`TotalOn_comp`): total functions compose into total functions.

A **refinement type** is a value paired with a proof that it satisfies a predicate — the sigma type
`{ a : α // P a }`:

```lean
def Refined (α : Type) (P : α → Prop) := { a : α // P a }
```

`TotalOn f` is exactly "`f` lifts to a total map on `Refined Par Closed`" (`totalOn_lifts_to_refined`).
Projecting the raw value and dropping the proof is the **type escape** the Rust port forbids.

## The Rust realization

In Rust, the sort and the refinements become *types*, so illegal states are unrepresentable:

- `Par<S>` carries the compile-time `NameSort`/`ProcSort` phantom sort; `quote`/`eval` recover `@`/`*`.
- `Closed` is a newtype (`TryFrom`/`new` to construct, `From<Closed> for Par` to discharge).
- `BindsAtMostOnce`, `WellScoped`, and the numeric refinements (`BlockHeight`, `SeqNum`, `Port`,
  `Hash32`, `NonNegI64`, …) are newtypes in [`shared/src/refined.rs`](../../../shared/src/refined.rs).

There is **no `Deref`/`.get()` escape** out of these newtypes — the invariant is structural, not a
convention. The machine gate that enforces this is
[`tools/audit-type-system.sh`](../../../tools/audit-type-system.sh): it fails the build on any production
`panic!`, `unsafe`, or silent fallible conversion.

> The full type-system spec, including the vertical layer map and the per-crate partiality catalogue,
> is [`spec/TYPE-SYSTEM.md`](../../../spec/TYPE-SYSTEM.md).
