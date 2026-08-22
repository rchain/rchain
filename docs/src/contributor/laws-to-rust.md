# The 19 laws → Rust code

A human-oriented walkthrough of how each of the [19 laws](../../../spec/INVENTORY.md) is realized as
**concrete Rust code** in this repository. This page exists because the mapping is obvious to a
machine but not to a person: most laws don't live in one obvious place, and a few of the type names in
the older docs were wrong or misleading. The canonical, terse table is
[`spec/INVENTORY.md`](../../../spec/INVENTORY.md); this page is the narrative version.

## How to read this

Three facts make the mapping intuitive once stated:

1. **The oracle is the spec, not the Scala.** The 19 laws and the ρ→CoC type discipline
   ([`spec/TYPE-SYSTEM.md`](../../../spec/TYPE-SYSTEM.md)) are what the Rust code must satisfy. The
   `legacy/` Scala tree is *reference material* for behavior, and the Scala tests are *differential*
   reference vectors — never the thing to reproduce bug-for-bug.
2. **Rust carries each law *structurally*.** Where Scala had a comment or a hand-maintained
   invariant, the Rust port encodes it in the type system — a refinement newtype (`NonNegI64`,
   `BlockHeight`, `Closed`), an `Ord`/`Eq` impl, a `Result` instead of a throw, or a
   `BTreeMap`/`BTreeSet` for determinism. So the "realization" of a law is usually a *type or a
   `derive`*, not a function that checks it at runtime.
3. **A law can span several files.** A single mathematical invariant (e.g. "content addressing",
   Law 16) is often split across the wire codec, the validator, and a fixed-width newtype. The table
   below names the *primary* anchor for each law and lists the supporting files.

The machine gate [`tools/audit-type-system.sh`](../../../tools/audit-type-system.sh) enforces the
cross-cutting "no silent partiality / no `unsafe`" discipline; it fails the build on any production
`panic!`/`unsafe`/silent-conversion.

---

## The table

| Law | What it means, in plain English | Where it lives in Rust | How it's tested |
|-----|--------------------------------|------------------------|-----------------|
| **1** | A `Par` (and `ESet`/`EMap`) is an *unordered* collection: the order of `\|`-joined processes, list fields, and map entries must not matter. "Sorting" a `Par` is canonicalization — sort twice, get the same thing; sort `p\|q` and `q\|p` the same. | `models/src/sorter.rs` — `sort_par` (`:704`) and `sort_par_term` (`:760`); the canonical form is carried structurally by `Sorted<Par<S>>` (`models/src/sorted.rs:26`, canonical `Eq`/`Ord`/`Hash`/`Serialize`) | `cargo test -p rchain-models sorter` (property test `sort_par_merge_commutes`) |
| **2** | Two processes are α/name-equivalent when their *sorted* canonical forms are structurally equal; `@` (quote) and `*` (eval) round-trip. There is **no runtime α-equivalence function** — it *is* equality on the sorted `Par`. | `models/src/ast.rs` — `Eq`/`Ord` derives (`:130`) + the phantom-sort aliases `Name = Par<NameSort>` / `Proc = Par<ProcSort>` and `quote`/`eval` (`:148`); sort classification in `models/src/types.rs` | same as Law 1 (equality on sorted form is the test) |
| **3** | Substitution is capture-avoiding (de Bruijn indices) and commutes with sorting: `sort(subst t) = subst(sort t)`. | `rholang/src/substitute.rs` — `substitute_par` (`:165`), `substitute_par_no_sort` (`:117`), `substitute_par_and_charge` (`:171`) | `cargo test -p rchain-rholang` + differential vs Scala golden vectors |
| **4** | Reduction is the COMM rule (`x!(…) \| for(… ← x){…} ⟶ …`), deterministic (first match wins), and `new` yields genuinely fresh unforgeable names. | `rholang/src/reduce.rs` — `DebruijnInterpreter` (`:1196`), `eval`/`eval_send`/`eval_receive`/`eval_new`; the actual tuple-space hand-off is `rspace/src/rspace.rs` produce/consume | `rholang/tests/execution.rs` (in-process runtime) |
| **5** | Spatial matching is decidable, and a receive/match pattern binds each free variable **at most once**. | `rholang/src/matcher/spatial_matcher.rs` — `spatial_match` (`:167`); the "bind at most once" invariant is carried by the `free_count: FreeCount` fields on `ReceiveBind`/`MatchCase` (`models/src/ast.rs:217,247`) | `cargo test -p rchain-rholang matcher` |
| **6** | A program (a deploy) has no globally free variables. | `models/src/types.rs` — the `Closed` newtype (`:340`, `Closed::new` `:345`) | `cargo test -p rchain-models` (type-level: you can't build a deploy from an unclosed term) |
| **7** | Joining channels commutes — the join key is the channels hashed in **sorted** order, so `x\|y` and `y\|x` hash the same. | `rspace/src/hashing/stable_hash_provider.rs` — `hash_seq` (`:23`), `hash_channels` (`:33`) | `cargo test -p rchain-rspace` (`hash_seq_sorts_channel_hashes`) |
| **8** | A COMM event is deterministic and content-addressed: produce refs are sorted and the event is a fixed structure. | `rspace/src/rspace.rs` — `produce` (`:454`), `consume` (`:432`); the `Comm`/`Produce`/`Consume` event structs in `rspace/src/trace/event.rs` | `cargo test -p rchain-rspace` (event round-trip) |
| **9** | Merging state-channel changes is a monoid: associative, and two *non-conflicting* change logs commute. | `rspace/src/merger/state_change_merger.rs` — `compute_trie_actions` (`:66`); supporting logic in `rspace/src/merger/{state_change,event_log_merging_logic,channel_change,event_log_index}.rs` | `cargo test -p rchain-rspace merger` |
| **10** | The state is a content-addressed Merkle radix trie: collision-free, with a well-defined empty root. | `rspace/src/history/radix_tree.rs` — `type Node = [Item; 256]` (`:35`), `RadixTreeImpl` (`:145`), `empty_root_hash` (`:43`) | `cargo test -p rchain-rspace history` |
| **11** | Replaying a block recomputes the same COMM events; the recomputed set is a subset of the recorded trace. | `rspace/src/replay_rspace.rs` — `ReplayRSpace` (`:83`), `impl IReplaySpace` (`:516`) | `cargo test -p rchain-rspace replay` |
| **12** | Actor atomicity (single-threaded `mbox.nextMsg`). | **Orphaned** — the Rosette VM (`rosette/`/`roscala/`) is out of scope; the Rust reducer (`rholang::reduce`) *replaces* the VM, so this law is not formalized in Rust. | — |
| **13** | Reflection: everything is an `Ob`, with meta/parent chains and a fork-join barrier. | **Orphaned** (same as Law 12). | — |
| **14** | Finality requires **strictly > 2/3** of bonded stake; the fringe is one message per bonded validator (an antichain). | `sdk/src/consensus.rs` — `is_super_majority` (`:15`, exact `3·stake > 2·total` in `i128`); the fringe/estimator in `block-storage/src/dag/finalizer.rs` (`calculate_finalization`); bonds as `BTreeMap<S, NonNegI64>` | `cargo test -p rchain-sdk consensus` + `casper/tests/finalization.rs` |
| **15** | The fringe is monotone by height and the seen-set never regresses (a finalized block stays seen). | `block-storage/src/dag/finalizer.rs` — `struct Message` (`:23`, `height: BlockHeight`, `sender_seq: SeqNum`); `block-storage/src/dag/message_state.rs` `insert_msg` (`:68`); `casper/src/validate.rs` `justification_regressions` (`:157`) | `casper/tests/multinode.rs` (seen-set convergence) |
| **16** | A block's number is max(parent)+1, `seqNum` increments by 1, and the block **hash** is `Blake2b256` of the block *minus* `{hash, sig}` (content addressing). | `casper/src/validate.rs` — `block_hash` (`:31`), `block_number` (`:113`), `sender_seq` (`:134`); `casper/src/proto_util.rs` `hash_block` (`:56`); the fixed-width `BlockHash`/`StateHash` newtypes (`models/src/{block_hash,block/state_hash}.rs`) | `cargo test -p rchain-casper validate` |
| **17** | Merge conflict resolution is deterministic (unique min-cost rejection); numeric channels are non-negative/no-overflow; the RNG merge is commutative. | `shared/src/refined.rs` — `NonNegI64`; `crypto/src/hash/blake2b512_random.rs` `merge` (`:191`); `rholang/src/merging.rs` `NumberChannel`; min-cost rejection in `sdk/src/dag/merging.rs` | `cargo test -p rchain-sdk -p rchain-rholang` |
| **18** | The height map is contiguous (no holes) and the fringe's identity is order-independent. | `block-storage/src/dag/metadata_store.rs` — `validate_dag_state` (`:77`) | `cargo test -p rchain-block-storage` |
| **19** | Blake2b256 is a canonical hash; `Blake2b512Random` has an associative splittable merge; secp256k1 signs/verifies; Curve25519 round-trips. | `crypto/src/hash/blake2b256_hash.rs`, `crypto/src/hash/blake2b512_random.rs`, `crypto/src/signatures/secp256k1.rs`, `crypto/src/encryption/curve25519.rs` | `cargo test -p rchain-crypto` (known-answer vectors) |

---

## Three laws worth a closer look

### Law 2 — why α-equivalence is "just" sorted-`Par` equality

There is no function called `alpha_equiv`. Instead, the *flat* `Par` struct (`models/src/ast.rs`)
stores processes as sorted lists of sub-terms, and the `Eq`/`Ord` impls on `Par` *are* the equivalence
relation. The `@`/`*` quote/eval distinction is recovered at the type level: `Name` is
`Par<NameSort>`, `Proc` is `Par<ProcSort>`, and `quote : Par<ProcSort> → Par<NameSort>` / `eval` invert
it. So "two terms are equivalent" is literally `a == b` after normalization, and the sorter (Law 1)
is what makes that equality order-insensitive.

### Law 8 — COMM is a produce + a consume, made deterministic

A COMM isn't a single function — it's the meet of a `produce` and a `consume` in `rspace/src/rspace.rs`.
The determinism requirement is met by (a) keeping produces sorted by their channel hash, and (b)
emitting a fixed `Comm { consume, produces, peeks }` struct (`rspace/src/trace/event.rs`). To see the
law, look at `produce`/`consume` together with the `Comm` struct — the sorted-produce invariant is what
makes the event content-addressed and replayable (Law 11).

### Law 16 — content addressing is "hash the block, minus its hash and signature"

The block hash is computed by `casper/src/proto_util.rs::hash_block`: it takes the block, blanks the
`hash` and `sig` fields, serializes the rest, and hashes it with `Blake2b256`. The *validator*
(`casper/src/validate.rs::block_hash`) re-checks that a received block's declared hash matches this
recomputation — that's the "content addressing" that makes any mutation detectable. The fixed-width
`BlockHash`/`StateHash` newtypes (`models/src/`) are what make "a 32-byte hash" a type-level guarantee
rather than a `Vec<u8>`.

---

## Verifying a law yourself

- **Find the test**: the "How it's tested" column names a `cargo test -p <crate> <filter>`; the
  property/differential tests live in `#[cfg(test)]` modules next to the code (or in `<crate>/tests/`).
- **Watch the invariant, not the assertion**: for the type-carried laws (3, 5, 6, 14, 17), the
  "realization" is the *type* — so the strongest check is often that a violating value *doesn't
  compile* (e.g. you can't build a `Closed` term from one with free variables, or a `NonNegI64` from a
  negative number).
- **Run the machine gate**: `tools/audit-type-system.sh` confirms zero production
  `panic!`/`unsafe`/silent-conversion — the cross-cutting discipline that underlies all 19 laws.

The canonical (terse) version of this mapping, with per-law Scala source-of-truth and Lean targets, is
[`spec/INVENTORY.md`](../../../spec/INVENTORY.md). The formal type discipline is
[`spec/TYPE-SYSTEM.md`](../../../spec/TYPE-SYSTEM.md).
