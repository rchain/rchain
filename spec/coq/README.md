# RChain formal specification — Coq track (Phase 0)

The Coq formalization, parallel to the Lean 4 track in [`../`](../). Coq is the home of the
**substitution / α-equivalence / programming-language metatheory** (capture-avoiding de Bruijn
substitution, α-equivalence), while Lean 4 covers the algebraic/order laws. The two tracks define the
same core `Proc` syntax and the same canonicalization `sort`, so their Phase-0 skeletons are
structurally identical.

## Building

Requires **Coq 8.x** (the files use only the standard library — `Arith`, `ZArith`, `String`, `List`;
no `Autosubst`/`ssreflect` yet).

```sh
make                 # = coq_makefile -f _CoqProject -o CoqMakefile && make -f CoqMakefile
```

Install Coq if needed: `opam install coq` (or `apt install coq`).

## Status

`Syntax.v` defines the flat `Par` ADT; `Sort.v` declares the canonical order as **axioms** (`cmpPar`,
`sortPar`, `sortPar_idempotent`, `sortPar_comm`). `Laws.v` states Laws 2–6 (`alpha_equiv`, `substPar`
with `subst_commutes_sort`, `reduce`, `spatial_matches` + `binds_at_most_once`, `closed` +
`closed_decidable`) as **axioms** — Coq owns the *definitions* (capture-avoiding de Bruijn
substitution, α-equivalence, Autosubst-style), which remain the Phase-1 obligations. This mirrors the
Lean track: Law 1's 30 element-comparator `axiom`s in [`../Rchain/Sort.lean`](../Rchain/Sort.lean), and
the stated-but-not-defined laws in `Rchain/{Subst,Reduce,Match,FreeVars}.lean`.

## Mapping to the Scala source of truth

| Coq | Scala |
|-----|-------|
| `score` / `cmpProc` / `parPair` | `models/.../rholang/sorter/ScoreTree.scala`, `ordering.scala` |
| `sort` | `models/.../rholang/sorter/ordering.scala` |
| `Proc` (binary Phase-0 form) | `models/src/main/protobuf/RhoTypes.proto` (`Par`/`Send`/`Receive`/`New`/`Match`) |

See [`../INVENTORY.md`](../INVENTORY.md) for the full 19-law catalog.
