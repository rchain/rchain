# Grammar and sorts

This is the precise statement of the rholang grammar. It is the reflective higher-order ρ-calculus of
Meredith & Radestock (2005), with its two sorts made explicit. The authoritative source is
[`spec/RHO-CALCULUS.md`](../../../spec/RHO-CALCULUS.md); this page reproduces the grammar so the rest of
Part II can refer to it.

## The grammar

The calculus is **reflective**: `@` quotes a process into a name, and `*` evaluates a name into a
process. Every term has exactly one of two sorts.

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

A **name** is a term usable in name position (the channel of a send, the pattern/source of a receive);
a **process** is a term with a top-level send/receive/new/match/replication. The reflective `@`/`*`
make the two sorts mutually recursive but *not identical*: `@ : Proc → Name`, `* : Name → Proc`.

## The sort judgment

`PSort` is the one genuine type distinction in rholang:

```lean
inductive PSort where | proc | name   -- spec/Rchain/Ty.lean
```

- `name` — usable in name position: `Nil`, grounds, expressions, bundles, unforgeables, connectives,
  and quoted processes.
- `proc` — usable in process position: anything with a top-level send/receive/new/match.

The judgment `Γ ⊢ t : s` has two halves, both **functional** and **decidable** (Fundamental F1):

- **variable level** — `HasVarSort Γ v s := varSort Γ v = some s`, where `Ctx := List PSort` and
  `varSort` classifies a de Bruijn `Var.bound l` by `Γ.get? l` (free/wildcard are unclassified).
- **term level** — `HasSort t s := classify t = s`, where `classify : Par → PSort` is structural: a
  `Par` with empty `sends`/`receives`/`news`/`matches` is a `name`, else a `proc`.

## The flat canonical form (Law 1)

The object-level `Par` is a **flat record of eight repeated fields** —
`sends` / `receives` / `news` / `exprs` / `matches` / `unforgeables` / `bundles` / `connectives` — each
kept sorted by the canonical order:

```
sort(sort p) = sort p          (idempotent)
sort(p | q)  = sort(q | p)     (commutative)
```

The flat form **erases** `@`/`*`: a `Par` in name position *is* a name. The reflective core is recovered
in the type layer (`classify`), not by extra `Par` constructors.

**Design decision (recorded):** the flat `Par` stays the canonical representation (Law 1 depends on
it), and the sort becomes a **phantom type parameter** `Par<S>` for `S ∈ {NameSort, ProcSort}`, with
`quote : Par<ProcSort> → Par<NameSort>` and `eval : Par<NameSort> → Par<ProcSort>` recovering `@`/`*`.
The sort is thus **compile-time** while the canonical sorted form is preserved.

## The three refinements

Three sigma-type refinements make the interpreter's partiality impossible:

- **`Closed p`** (Law 6) — no free variables; decidable; preserved by composition, `≡`,
  canonicalization, and `⟶`.
- **`WellScoped Γ t`** — every bound level of `t` is within `Γ` (the variable half of the judgment).
- **`BindsAtMostOnce`** (Law 5) — a pattern binds each free variable at most once (the `freeCount`
  fields of `ReceiveBind`/`MatchCase`).

## Where each piece lives

| Concept | Lean | Rust |
|---|---|---|
| flat `Par` (8 fields) | [`spec/Rchain/Par.lean`](../../../spec/Rchain/Par.lean) | `models/src/ast.rs` |
| `PSort` / `classify` / `Closed` | [`spec/Rchain/Ty.lean`](../../../spec/Rchain/Ty.lean) | `models/src/types.rs` |
| `StrCong` `≡`, `Reduce` `⟶` | [`spec/Rchain/Rho.lean`](../../../spec/Rchain/Rho.lean) | `rholang/src/reduce.rs` |
| canonical `sort` (Law 1) | [`spec/Rchain/Sort.lean`](../../../spec/Rchain/Sort.lean) | `models/src/sorter.rs` |
| substitutions / matching / freshness | [`spec/Rchain/{Subst,Reduce,Match,FreeVars}.lean`](../../../spec/Rchain/) | `rholang/src/{substitute,matcher}.rs` |
