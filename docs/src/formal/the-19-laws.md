# The 19 laws

RChain's behavior is pinned by **19 laws** — one invariant per layer of the system. Each law maps a
language or system feature to its formalization: a Lean theorem, a Coq axiom, the executable K rule,
and the Rust realization. The canonical catalog is
[`spec/INVENTORY.md`](../../../spec/INVENTORY.md); this page is the reader-facing rendering of the same
mapping.

Legend: **proven** = a theorem with a proof; **stated** = an axiom with a precise signature (definition
deferred); **axiom** = postulated by design (a cryptographic primitive).

## Rholang — the language (Laws 1–6)

| Law | Invariant | Surface feature | Lean | Coq | K rule |
|---|---|---|---|---|---|
| **1** | canonicalization is idempotent & commutative: `sort(sort p)=sort p`, `sort(p\|q)=sort(q\|p)` | the sorted `Par`, commutative `ESet`/`EMap` | `Sort.lean` — `sortPar_idempotent` / `sortPar_comm` (**proven**, mod 30 element-comparator axioms) | `Sort.v` — `sortPar_idempotent` / `sortPar_comm` (axioms) | normalization (α + canonical `\|` sort) |
| **2** | α / name equivalence: par order, `\| Nil`, associativity, top-level arithmetic, α, `@`/`*` | `@`/`*`, `@{P\|Q}=@{Q\|P}` | `Rho.lean` — `StrCong` `≡` (**proven** core) | `Laws.v` — `alpha_equiv` (axiom) | `name-equivalence.k`, `alpha-equivalence.k` |
| **3** | capture-avoiding de Bruijn substitution; `sort(subst t)=subst(sort t)` | variable binding | `Subst.lean` — `substPar`, `sort_subst`, `subst_closed` (**stated**) | `Laws.v` — `substPar`, `subst_commutes_sort` (axiom) | `free.k` (substitution; `substitution.k` referenced) |
| **4** | reduction (COMM), first-match-wins, `new` freshness | send/receive, `match` | `Rho.lean` `Reduce` ⟶ (**proven** core) + `Reduce.lean` `reduce_deterministic`/`reduce_freeVars_subset` (**stated**) | `Laws.v` — `reduce` (axiom) | `processes-semantics.k`, `sending-receiving.k`, `persistent-sending-receiving.k` |
| **5** | spatial matching; a free var bound at most once | patterns, `_`, `~`, `/\`, `\/` | `Match.lean` — `BindsAtMostOnce`, `spatialMatches`, `spatialMatches_decidable` (**stated**) | `Laws.v` — `spatial_matches`, `binds_at_most_once` (axiom) | `matching-function.k`, `specific-matching-rules.k`, `exact-matching-function.k`, `matching-with-par.k` |
| **6** | no globally free variables | `Closed` | `Ty.lean` `Closed` (**proven**) + `FreeVars.lean` `freeVarOf`/`closed_iff_no_freeVars` | `Laws.v` — `closed`, `closed_decidable` (axiom) | `free.k`, `program-restrictions.k` |

## RSpace — the tuple space (Laws 7–11)

| Law | Invariant | Feature | Lean |
|---|---|---|---|
| **7** | join commutativity (channel keys hashed in sorted order) | multi-channel receive | `RSpace/Join.lean` — `joinKey_perm` (**stated**) |
| **8** | deterministic COMM (produce refs sorted; content-addressed events) | the comm event | `RSpace/Comm.lean` — `comm_content_addressed` (**stated**) |
| **9** | merge is a monoid; non-conflicting logs commute | state merging | `RSpace/Merge.lean` — `mergeChanges_assoc`/`comm` (**stated**) |
| **10** | Merkle determinism (content-addressed radix trie, collision-free, empty root) | history | `RSpace/Merkle.lean` — `trie_collision_free`/`trie_empty_root` (**stated**) |
| **11** | replay determinism (recomputed COMM ⊆ recorded trace) | replay | `RSpace/Comm.lean` — `replay_comm_subset` (**stated**) |

## Rosette — the actor VM (Laws 12–13)

| Law | Invariant | Status |
|---|---|---|
| **12** | actor atomicity (single-threaded `mbox.nextMsg`) | **orphaned** — `rosette`/`roscala` VM is out of scope |
| **13** | reflection (everything is an `Ob`; meta/parent chain; fork-join barrier) | **orphaned** |

## Casper / Storage / Crypto (Laws 14–19)

| Law | Invariant | Feature | Lean |
|---|---|---|---|
| **14** | finality requires **> 2/3** bonded stake; fringe = one message per validator | the fringe | `Casper/Stake.lean` `isSuperMajority` (`3·stake > 2·total`), `finality_iff_supermajority`; `Casper/Fringe.lean` `fringe_antichain` (**stated**) |
| **15** | fringe monotone by height; seen-set monotone | the DAG | `Casper/Fringe.lean` — `fringe_monotone`, `seen_monotone` (**stated**) |
| **16** | block number = max(parent)+1; seqNum strictly +1; content addressing; bonds cache = PoS | blocks | `Casper/Validate.lean` — `block_number_max_parent_plus_one`, `seq_num_strictly_increases`, `content_addressing` (**stated**) |
| **17** | merge determinism; numeric channels non-negative | merges | `Casper/Validate.lean` — `numeric_channels_nonneg` (**stated**) |
| **18** | height map contiguous; fringe identity order-independent | storage | `Casper/Validate.lean` — `height_map_contiguous`, `fringe_identity_order_independent` (**stated**) |
| **19** | Blake2b256 canonical; `Blake2b512Random` associative splittable merge; sig verify/sign; Curve25519 round-trip | crypto | `Crypto/Random.lean` `mergeRandom_assoc`/`comm`; `Crypto/Spec.lean` `blake2b256_collision_free`, `sign_verify_roundtrip`, `curve25519_roundtrip` (**axiom**, by design) |

## Reading the formalization

- **Lean** (`spec/Rchain/*.lean`) owns the algebraic/order laws and the type-system fundamentals. Build:
  `cd spec && lake build`.
- **Coq** (`spec/coq/*.v`) owns the substitution / α-equivalence metatheory (Laws 2–6). Build:
  `make -C spec/coq`.
- **K** (`legacy/rholang/src/main/k/rholang/*.k`) is the executable reference semantics of the language.
- **Rust** carries each invariant *structurally* (refinement newtypes, no silent partiality); the
  machine gate is `tools/audit-type-system.sh`.

Per-law status, source-of-truth pointers, and Rust realization are in
[`spec/INVENTORY.md`](../../../spec/INVENTORY.md).
