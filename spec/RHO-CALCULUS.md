# The reflective higher-order ρ-calculus — core specification

This document is the **core** of the formal specification. It defines the reflective ρ-calculus
(Meredith & Radestock 2005) that the node executes, with its two sorts made explicit, and records
how the Rust port realizes it as a **safe-by-construction** type system — a Calculus of
Constructions whose base sort is the ρ-calculus itself (see [`TYPE-SYSTEM.md`](TYPE-SYSTEM.md)).

It is the foundation the 19 laws in [`INVENTORY.md`](INVENTORY.md) are stated *over*. The
machine-checked realization lives in [`Rchain/Par.lean`](Rchain/Par.lean), [`Rchain/Rho.lean`](Rchain/Rho.lean),
and [`Rchain/Ty.lean`](Rchain/Ty.lean).

---

## 1. The grammar (two sorts, explicitly)

The calculus is **reflective**: a name is a quoted process (`@`), and a process can evaluate a name
(`*`). The grammar is *sorted* — every term belongs to exactly one of the two sorts:

```
Name  ::=  @Proc                        (quote a process into a name)
        |  Nil                          (the empty name / stopped process)
        |  Ground                       (GBool | GInt | GBigInt | GString | GUri | GByteArray)
        |  Expr                         (EVar | EList | ETuple | ESet | EMap | arithmetic | EMethod)
        |  Bundle                       (read/write capability)
        |  Unforgeable                  (GPrivate | GDeployId | GDeployerId | GSysAuthToken)
        |  Connective                   (ConnAnd | ConnOr | ConnNot | VarRef | ConnBool | …)

Proc  ::=  *Name                        (evaluate a name into a process)
        |  Name!(Name, …)               (send; `!!` = persistent)
        |  for( Name ← Name, … ){ Proc }   (receive; `<=` = peek; a name may be a binder pattern)
        |  new … in Proc                (restriction: fresh unforgeable names)
        |  match Name { Name ⇒ Proc, … }   (spatial matching, first-match-wins)
        |  Proc | Proc                  (parallel composition)
        |  !Proc                        (replication)
```

A **name** is a term usable in *name* position (channel position of a send, pattern/source of a
receive); a **process** is a term with a top-level send/receive/new/match/replication. The
reflective `@`/`*` make the two sorts mutually recursive but *not* identical: `@` injects
`Proc → Name`, `*` injects `Name → Proc`.

## 2. The sort judgment

`PSort` is the one genuine rholang type distinction:

```lean
inductive PSort where | proc | name   -- Rchain/Ty.lean
```

- `name` — a term usable in name position (pure names: `Nil`, grounds, expressions, bundles,
  unforgeables, connectives, and quoted processes).
- `proc` — a term usable in process position (anything with a top-level send/receive/new/match).

The judgment `Γ ⊢ t : s` has two halves (both **functional** and **decidable**, Fundamental F1):

- variable level — `HasVarSort Γ v s := varSort Γ v = some s`, where `Ctx := List PSort` and
  `varSort` classifies a de Bruijn `Var.bound l` by `Γ.get? l` (free/wildcard are unclassified);
- term level — `HasSort t s := classify t = s`, where `classify : Par → PSort` is the structural
  sort (a `Par` with empty `sends`/`receives`/`news`/`matches` is a `name`, else a `proc`).

## 3. The flat canonical form (Law 1)

The object-level `Par` is a **flat record of eight repeated fields**
(`sends`/`receives`/`news`/`exprs`/`matches`/`unforgeables`/`bundles`/`connectives`), each kept
sorted by the canonical order (Law 1: `sort(sort p) = sort p`, `sort(p|q) = sort(q|p)`). The flat
form **erases** the `@`/`*` distinction — a `Par` in name position *is* a name — so the reflective
core is **recovered in the type layer** (`classify`), not by extra `Par` constructors.

**Design decision (recorded):** the flat `Par` remains the *canonical representation* (Law 1 depends
on it), and the sort becomes a **phantom type parameter** `Par<S>` for `S ∈ {NameSort, ProcSort}`,
with `quote : Par<ProcSort> → Par<NameSort>` and `eval : Par<NameSort> → Par<ProcSort>` recovering
`@`/`*`. The sort is thus **compile-time** while the canonical sorted form is preserved. See §6.

## 4. Structural operations

Each is a named rule; the Lean spellings are in [`Rchain/Rho.lean`](Rchain/Rho.lean).

| Operation | Definition | Law |
|---|---|---|
| Structural congruence `≡` | par-order (`p|q ≡ q|p`), `p | Nil ≡ p`, associativity, congruence under every constructor | Law 2 (α/name equivalence) |
| Substitution `subst σ p` | minimal simultaneous capture-avoiding substitution (de Bruijn); `sort(subst t) = subst(sort t)` | Law 3 |
| Reduction `⟶` | COMM: `Name!(x) | for(Name ← …){P} ⟶ P[subst]`; congruence under `|`; replication `!`/`!!`; `new` freshness | Law 4 |
| Spatial matching | pattern `Name` matches a process structurally; a free variable is bound **at most once** | Law 5 |
| Closedness | no free variables in a program | Law 6 |

## 5. Refinements (the sigma types)

Three refinements make the interpreter's partiality impossible. In the Rust port each is a
**newtype carrying the invariant** (a sigma type `{ t : T | P t }`; see
[`TYPE-SYSTEM.md`](TYPE-SYSTEM.md) §1.7):

- **`Closed p`** (Law 6) — no free variables; decidable via the `closed*` family; preserved by
  composition, `≡`, canonicalization, and `⟶` (Fundamental F4).
- **`WellScoped Γ t`** — every bound level of `t` is within `Γ` (the variable half of the judgment,
  `HasVarSort`).
- **`BindsAtMostOnce`** (Law 5) — a pattern binds each free variable at most once; carried by the
  `freeCount` fields of `ReceiveBind`/`MatchCase`.

## 6. The Rust realization — safe by construction

The ρ-calculus is the **base sort** of a Calculus of Constructions. In Rust, the sort and the
refinements become *types*, so illegal states are unrepresentable:

| ρ-calculus concept | Rust realization |
|---|---|
| `PSort` | phantom sorts `NameSort`/`ProcSort` parameterizing the flat `Par<S>` |
| `@` / `*` | `quote : Par<ProcSort> → Par<NameSort>`, `eval : Par<NameSort> → Par<ProcSort>` |
| `classify`/`is_pure_name` | `TryFrom<Par> for Name` (validated, at the boundary); the sort is then carried, not re-checked |
| `Closed` (Law 6) | `Closed(Par)` newtype (`TryFrom`/`new` construction, `From<Closed> for Par` discharge) |
| `WellScoped` | newtype over `(Ctx, Par)` |
| `BindsAtMostOnce` (Law 5) | newtype carrying the `freeCount` invariant |
| numeric invariants | `BlockHeight`/`SeqNum`/`Port`/`WireLen`/`Hash32`/`NonNegI64` in [`../shared/src/refined.rs`](../shared/src/refined.rs) (`ByteLen`/`ShortLen` reserved for 1c) |

The sorter, wire codec, and constructors (`RhoName`/`RhoNil`/`single_expr`/`single_unforgeable`) are
sort-typed; the interpreter (`rholang/`) and tuple space (`rspace/`) thread `Par<S>` end-to-end.
