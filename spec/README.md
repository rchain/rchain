# RChain formal specification (Phase 0)

A machine-checked specification of the mathematical invariants that govern the RChain node, written
as a [Lean 4](https://lean-lang.org/) formalization. It is the source of truth the Rust rewrite is
written against: every law here maps to a Rust property/differential test in later phases.

## Why

The full motivation — memory safety and the calculus-native expression of the node (λ → π → ρ →
Calculus of Constructions) — is in [`../docs/src/contributor/why-rust.md`](../docs/src/contributor/why-rust.md). The node
(Scala + a C++ Rosette VM) is broadly sound, so the rewrite is not a correctness repair. This spec's
job is therefore **preservation under translation** — pin down the invariants so the port cannot
silently drop them.

## Building

```sh
cd spec
lake build        # compiles the formalization
```

The build requires **Mathlib** (pinned to `v4.12.0` in `lakefile.toml`); the `.lean` files already
import `Mathlib.Data.*`/`Mathlib.Order.*` for `Multiset`/`Finset`/`Order`.

## Layout

```
spec/
  Rchain.lean          root module (imports everything)
  Rchain/
    Syntax.lean        Ground / Var — the core Rholang scalar ADT (de Bruijn levels)
    Par.lean           the flat `Par` ADT (8 list fields) + `nilPar`/`parMerge`
    Cmp.lean           the `Comparator` scaffold + `sortList`
    Rho.lean           Law 2 core (`StrCong` ≡) + Law 4 core (`Reduce` ⟶ COMM)
    Sort.lean          Law 1: canonicalization `sortPar` + `sortPar_idempotent`/`sortPar_comm`
    Ty.lean            the CoC layer: `PSort`/`Closed` (Law 6) + the proven fundamentals
    Subst.lean         Law 3: capture-avoiding substitution (`sort ∘ subst` commute)
    Reduce.lean        Law 4: determinism + `new` freshness
    Match.lean         Law 5: `BindsAtMostOnce` + decidable spatial matching
    FreeVars.lean      Law 6: `freeVarOf` + `Closed ↔ no free vars`
    RSpace/            Laws 7–11: Join/Comm/Merge/Merkle (stated)
    Casper/            Laws 14–18: Stake/Fringe/Validate (stated)
    Crypto/            Law 19: Random/Spec (axiomatized by design)
  INVENTORY.md         the 19-law catalog: source-of-truth → theorem → Rust test
```

Laws 12–13 (Rosette) are **orphaned**: the `rosette`/`roscala` VM is out of scope (not wired into
`build.sbt`); they are documented in `INVENTORY.md` but have no Lean files.

## Proven vs stated

- **Proven**: Law 1 (`sortPar_idempotent`, `sortPar_comm` in `Rchain/Sort.lean`), Law 2's core
  (`StrCong` ≡ in `Rchain/Rho.lean`), Law 4's core (`Reduce` ⟶ COMM in `Rchain/Rho.lean` +
  `reduce_closed` in `Rchain/Ty.lean`), and Law 6 (`Closed` + the preservation fundamentals in
  `Rchain/Ty.lean`). The one residual of Law 1 is the lawfulness of the 10 element comparators
  (`cmpPar`/`cmpSend`/…/`cmpConnective`), declared as 30
  `cmpX_eq_iff`/`cmpX_swap`/`cmpX_lt_trans` axioms in `Rchain/Sort.lean` (the 12 list-comparator and
  `cmpGUnforgeable` laws are proven by direct induction). The remaining element laws need mutual
  induction over the AST, which hangs Lean's termination checker for the two-argument `cmpX` family.
  The sum-type `Sortable`/`cmpSortable` definition is in place (termination proven); the remaining step
  is its laws proof by well-founded induction — see the note in `Rchain/Sort.lean`.
- **Stated** (axiom, precise signature, definition deferred): Laws 3 (`Subst.lean`), 4-full
  (`Reduce.lean`), 5 (`Match.lean`), 7–11 (`RSpace/*`), 14–18 (`Casper/*`). Each states the law's
  signature over the `Par`/abstract data types; the definitions (capture-avoiding substitution,
  α-equivalence) are Coq's obligation, the RSpace/Casper definitions are later phases.
- **Axiomatized** (never proven, by design): Law 19's cryptographic primitives (Blake2b, secp256k1,
  Curve25519) are modeled as abstract interfaces whose required properties are *postulated*
  (`Crypto/Random.lean`, `Crypto/Spec.lean`). Proving real crypto is out of scope.

## How laws drive the Rust port

| Spec artifact | Rust counterpart |
|---------------|------------------|
| `Sort.sort` (canonical form) | `Sorted<Par>` (canonical `Eq`/`Ord`/`Hash`/`Serialize`) |
| a proven law `L` | a `proptest`/`quickcheck` property asserting `L`, plus a differential test feeding identical inputs to the Scala node and comparing state hashes |
| `Syntax.Proc` (the ADT) | reference for the Rust data model and its `Eq`/`Hash` derivations |
| axiomatized crypto interfaces | Rust traits pinned by known-answer vectors |

## Ground truth

The laws are validated against — not replacing — the Scala tests that already encode them:

- `rholang/src/test/scala/coop/rchain/rholang/interpreter/{ReduceSpec,ReplaySpec}.scala`
- `models/src/test/scala/coop/rchain/models/rholang/SortTest.scala`
- `node/src/test/scala/coop/rchain/node/mergeablity/MergeabilityRules.scala`
- `casper/src/test/scala/coop/rchain/casper/batch1/MultiParentCasperReportingSpec.scala`
