# RChain invariant inventory

The catalog of mathematical invariants the Rust rewrite must preserve. Each row links a law to its
source-of-truth location, its Lean formalization, and its Rust type realization; a human-facing
walkthrough of each law → the concrete Rust file/type/function that carries it (and the test that
gates it) is in [`../docs/src/contributor/laws-to-rust.md`](../docs/src/contributor/laws-to-rust.md).

**Type system.** The port's own type discipline — the ρ-calculus embedded as the base sort of a
Calculus of Constructions, with no silent partiality — is specified in [`TYPE-SYSTEM.md`](TYPE-SYSTEM.md),
guided by `Rchain/Rho.lean` and `Rchain/Ty.lean`. It is **not** a new law and does not pre-empt Laws
1–19 below.

**Status legend**

- **stated** — theorem statement exists in Lean (Phase 0), proof is a later obligation
- **Phase N** — to be formalized and proven in phase N (1 = execution core, 2 = RSpace, 3 = Rosette,
  4 = Casper, 5 = crypto/storage)
- **axiom** — postulated (cryptographic primitive), not proven, by design
- **residual axiom** — a proof obligation currently declared `axiom` (not by design), e.g. the
  comparator lawfulness in Law 1 (30 `cmpX_eq_iff`/`cmpX_swap`/`cmpX_lt_trans`)
- **open** — recorded question, not yet formalizable

## Laws

| # | Layer | Law | Source of truth | Rust type realization | Lean | Status |
|---|-------|-----|-----------------|----------------------|------|--------|
| 1 | Rholang | `Par`/`ESet`/`EMap` are commutative; canonicalization to a total order is **idempotent** (`sort(sort p) = sort p`); `sort(p\|q) = sort(q\|p)` | `models/src/main/scala/coop/rchain/models/rholang/sorter/ScoreTree.scala`, `ordering.scala`, `models/src/main/scala/coop/rchain/models/SortedParHashSet.scala` | `Sorted<Par<S>>` (canonical `Eq`/`Ord`/`Hash`/`Serialize`) + `sorter::sort_par` | `Rchain/Sort.lean` (`sortPar_idempotent`, `sortPar_comm`) | **proven** (`sortPar_idempotent` + `sortPar_comm` + leaf laws + `sortList` are proven); residual: the 10 element-comparator lawfulness (30 axioms; the 12 list-comparator and `cmpGUnforgeable` laws are discharged) is the remaining "total order" obligation |
| 2 | Rholang | **α/name equivalence** = par order + `\| Nil` + top-level arithmetic + α + added eval/quote | `rholang/src/main/k/rholang/name-equivalence.k:1-14`, `rholang/reference_doc/normalization_process/README.md` | `Par<S>` structural equality (`≡` = sorted `Par` + `quote`/`eval`) | `Rchain/Sort.lean` (≡ core in `Rho.lean`; deep α is Coq `Laws.v`) | **stated** (≡ core proven in `Rho.lean`) |
| 3 | Rholang | **Capture-avoiding de Bruijn substitution**; `sort(subst t) = subst(sort t)` | `rholang/src/main/scala/coop/rchain/rholang/interpreter/Substitute.scala`, `Env.scala` | `rholang::substitute::substitute_par` (total on `Closed`) | `Rchain/Subst.lean` (+ Coq `Laws.v`) | **stated** |
| 4 | Rholang | **Reduction (comm)**; first-match-wins; `new` yields fresh unforgeable names | `rholang/.../interpreter/Reduce.scala`, `rholang/src/main/k/rholang/*.k` | `rholang::reduce::DebruijnInterpreter` + `Tuplespace` (`Reduce`) | `Rchain/Reduce.lean` (COMM core in `Rho.lean`) | **stated** (COMM core proven in `Rho.lean`; determinism/freshness stated) |
| 5 | Rholang | **Spatial matching**; a free var is bound at most once (`addedVars.distinct`) | `rholang/.../interpreter/matcher/SpatialMatcher.scala`, `ParCount.scala` | `rholang::matcher::spatial_match` + `free_count: FreeCount` fields on `ReceiveBind`/`MatchCase` | `Rchain/Match.lean` (+ Coq `Laws.v`) | **stated** |
| 6 | Rholang | **No globally free variables** in a program | `rholang/src/main/k/rholang/{free,program-restrictions}.k`, `models/.../HasLocallyFree.scala` | `models::types::Closed` (newtype) | `Rchain/FreeVars.lean` (`Closed` in `Ty.lean`) | **proven** (`Ty.lean`) |
| 7 | RSpace | **Join commutativity** (channel keys hashed in sorted order) | `rspace/.../hashing/StableHashProvider.scala:18-22` | `rspace::hashing::StableHashProvider::hash_seq` (sorted) | `Rchain/RSpace/Join.lean` | **stated** |
| 8 | RSpace | **Deterministic COMM** (produce refs sorted; content-addressed events) | `rspace/.../trace/Event.scala:35-39` | `rspace::rspace` sorted produce + `Comm` event | `Rchain/RSpace/Comm.lean` | **stated** |
| 9 | RSpace | **Merge is a monoid**; non-conflicting logs commute | `rspace/.../merger/{StateChange,ChannelChange,EventLogMergingLogic}.scala` | `rspace::merger::state_change_merger` (`compute_trie_actions`) | `Rchain/RSpace/Merge.lean` | **stated** |
| 10 | RSpace | **Merkle determinism**: content-addressed radix trie, collision-free, empty-root | `rspace/.../history/RadixTree.scala:50-68` | `rspace::history::RadixTreeImpl` (`Node = [Item; 256]`) | `Rchain/RSpace/Merkle.lean` | **stated** |
| 11 | RSpace | **Replay determinism**: recomputed COMM ⊆ recorded trace | `rspace/.../ReplayRSpace.scala:68-71` | `rspace::ReplayRSpace` | `Rchain/RSpace/Comm.lean` | **stated** |
| 12 | Rosette | **Actor atomicity** (single-threaded `mbox.nextMsg`) | `rosette/README:27-35`, `roscala/.../ob/Actor.scala:52-61` | *deferred* (`rosette`/`roscala` orphaned) | *(none)* | **orphaned** (Rosette VM out of scope) |
| 13 | Rosette | **Reflection**: everything is an `Ob`; meta/parent chain; **fork-join barrier** | `roscala/.../ob/{Ob,Meta,Ctxt}.scala`, `Vm.scala:185-188` | *deferred* | *(none)* | **orphaned** (Rosette VM out of scope) |
| 14 | Casper | **Finality requires > 2/3** bonded stake; fringe = one message per bonded validator (antichain) | `sdk/.../consensus/Stake.scala:8`, `block-storage/.../dag/Finalizer.scala:76,133` | `sdk::consensus::is_super_majority` (exact `3·stake > 2·total`) + `BTreeMap<S, NonNegI64>` bonds | `Rchain/Casper/{Stake,Fringe}.lean` | **stated** |
| 15 | Casper | Fringe **monotone by height**; **seen-set monotone** (no regression) | `block-storage/.../dag/MessageMapSyntax.scala:33`, `casper/.../Validate.scala:285` | `BlockHeight`/`SeqNum` in `block-storage::dag::Message` | `Rchain/Casper/Fringe.lean` | **stated** |
| 16 | Casper | **Block number** = max(parent)+1; **seqNum** strictly +1; **content addressing** (`hash = Blake2b256(block−{hash,sig})`); **bonds cache = PoS state** | `casper/.../Validate.scala:177,249,256,360`, `util/ProtoUtil.scala:70` | `BlockHeight`/`SeqNum` + `BlockHash`/`StateHash` (over `Hash32`) | `Rchain/Casper/Validate.lean` | **stated** |
| 17 | Casper | **Merge determinism** (unique min-cost rejection); numeric channels non-negative/no-overflow; RNG merge commutative | `sdk/.../dag/merging/ConflictResolutionLogic.scala:200`, `rholang/.../merging/RholangMergingLogic.scala:90` | `NonNegI64` numeric channels; `Blake2b512Random` merge | `Rchain/Casper/Validate.lean` | **stated** |
| 18 | Storage | **Height map contiguous** (no holes); **fringe identity order-independent** | `block-storage/.../BlockMetadataStore.scala:156`, `models/.../FringeData.scala:32` | `BlockHeight` + `block-storage::dag::metadata_store::validate_dag_state` | `Rchain/Casper/Validate.lean` | **stated** |
| 19 | Crypto | Blake2b256 canonical hash; `Blake2b512Random` **associative splittable merge**; sig verify/sign; Curve25519 round-trip | `crypto/.../` (`Blake2b512Random`, `Secp256k1`, `Curve25519`) | `Blake2b256Hash` (over `Hash32`) + `Blake2b512Random` (**axiom**) | `Rchain/Crypto/{Random,Spec}.lean` | **axiom** (by design) |

## Open questions

1. `casper/src/main/resources/casper.tla` models only the genesis **bootstrap ceremony handshake**.
   The finality rule is now formalized in `CasperFinality.tla` (Laws 14/15/16: > 2/3 supermajority,
   fringe antichain + seen-set monotonicity, seqNum strictly increasing), reconstructed from
   `Finalizer.scala` + `MessageMapSyntax.scala`.
2. The `faultTolerance` field asserted in `integration-tests/test/test_dag_correctness.py:105-111`
   is not computed anywhere in this tree — its exact formula is declared an open question;
   `CasperFinality.tla` states the safety margin (`3·support − 2·total`) and the monotonicity the
   test relies on.
3. `Rosette` (`rosette/`, `roscala/`) is not wired into `build.sbt` and is **orphaned** — Laws 12–13
   are documented (no Lean files); the Rust reducer (`rholang::reduce`) replaces the VM, so these two
   laws are out of scope for the formalization.
