# Test-coverage audit & gap analysis

This register records an adversarial audit of the Rust port's test coverage. It is the durable
companion to [`AUDIT.md`](AUDIT.md) (the *code* findings register): `AUDIT.md` records what the code
does wrong; this page records what the tests fail to catch. The audit is re-runnable — the numbers
below are from `branch dev` after the rust-first reimplementation (Phases 0–5).

## Inventory

**~750 `#[test]`/`#[tokio::test]` functions** across the workspace (unit + 16 integration, 5
proptest, 7 bench — the per-crate table below is authoritative; the headline count drifts as tests are
added). Only **3 of 12 crates have integration tests** (`rholang`, `casper`, `node`).

| Crate | Unit | Integration | Property | Bench |
|---|---|---|---|---|
| `sdk` | 34 | — | — | — |
| `shared` | 76 | — | — | — |
| `crypto` | 49 | — | — | — |
| `graphz` | 11 | — | — | — |
| `models` | 95 | — | — | — |
| `block-storage` | 14 | — | — | — |
| `comm` | 51 | — | — | — |
| `rspace` | 42 | — | 5 | — |
| `rholang` | 61 | 7 | — | — |
| `casper` | 105 | 5 | — | — |
| `node` | 59 | 2 | — | — |
| `rspace-bench` | — | — | — | 7 |

- **Property tests**: only `rspace/src/property_tests.rs` (Laws 7–10: join commutativity, deterministic
  COMM, Merkle determinism, merge monoid).
- **Differential/golden**: `models` wire bitset, `rspace` scodec + stable-hash TSV, `rholang`
  execution post-state hashes, and crypto known-answer vectors — the Scala-ground-truth tests.
- **The 110 legacy `.rho`/`.rhox` contracts** under `legacy/` are **not referenced by any Rust test**;
  the parser/reducer are only exercised against inline strings (and, historically, the now-native
  genesis contracts).

## The 1 remaining `#[ignore]`d test

- **`casper/tests/finalization.rs:303` `round_robin_finalizes_common_prefix`** — a lockstep/round-robin
  DAG never advances the fringe (the Scala `MultiParentCasperFinalizationSpec` is itself `ignore`d).
  The finalizing shape is covered by `fork_structure_advances_fringe` (active).

*(The former second ignored test, `rholang/tests/execution.rs` `list_channel_matches`, was a
list-as-channel hash/equality bug that was resolved during the reimplementation; it is now
un-ignored and passing.)*

## Gap analysis (severity-ordered)

For each gap: **code location** → **current test state** → **the seam a regression test attaches to**.

- **G1 — Equivocation rejection is untested** (`casper/src/dag.rs:146-160`). The H-1 fix rejects a
  second block by the same sender reusing `seq_num`; zero tests exercise it. *Seam:*
  `BlockDagKeyValueStorage::insert` over the in-memory `build_storage()` helper.

- **G2 — DoS / resource limits are untested.** `RateLimiter` (`node/src/api/grpc/mod.rs:57`), the
  chunker underflow guard (`comm/src/transport/chunker.rs:43` `checked_sub`), and the dispatch
  semaphore (`comm/src/transport/grpc_transport_receiver.rs`, `MAX_CONCURRENT_DISPATCH`). *Seam:*
  each is a pure function or an `Arc<Semaphore>`.

- **G3 — PoS money-moving mutations are untested** (`rholang/src/native_state.rs:155-200`): `slash`,
  `pre_charge` (incl. the insufficient-funds `Err` branch), `refund`, `close_block`, vault
  `deposit`/`transfer`. Only the encode/decode helpers are tested. *Seam:*
  `NativeSystemState` over `InMemNativeStore::empty()`.

- **G4 — Gas-metering enforcement is under-tested.** `ChargingRSpace::produce/consume`
  (`rholang/src/storage.rs:106-131`) charge paths have no test; there is no end-to-end test that a
  deploy exceeding `phlo_limit` fails. *Seam:* `ChargingRSpace::new(space, cost)` with a tiny balance,
  and `RuntimeManager::process_deploy` with a low `phlo_limit`.

- **G5 — State-sync export/import has no round-trip test** (`rspace/src/state/*`). Only
  `MockExporter`/trivial single-leaf tests. *Seam:* real `RSpaceExporter` → real `RSpaceImporter`.

- **G6 — History checkpoint/reset/rollback is untested.** `history_repository.rs`, `roots_store.rs`,
  `root_repository.rs`, `checkpoint.rs`, and core `rspace/src/rspace.rs` have no `mod tests`. *Seam:*
  `RSpace::create_checkpoint`/`reset`/`create_soft_checkpoint`/`revert_to_soft_checkpoint`.

- **G7 — Replay is only tested on a trivial `@chan!(42)`.** `replay_rspace.rs`/`runtime_replay.rs`
  internals (`check_replay_data`) have no direct tests. *Seam:* non-trivial deploys (multi-channel
  COMM, persistent `!!`, peek `<<-`, native mutation) through `replay_compute_state`.

- **G8 — The Finalizer full loop is only indirectly tested.** `calculate_next_fringe_support_map`
  and `calculate_finalization` have no direct tests. *Seam:* the `msg()` helper in
  `finalizer.rs` tests.

- **G9 — Malformed-input rejection is partial.** `NodeIdentifier::from_hex`/`from_address`
  (`comm/src/peer_node.rs:25-31,125`) and `KeySegment` (`rspace/src/history/key_segment.rs`) reject
  bad input but have no negative tests. (`base16` and the validate layer are well covered.)

- **G10 — TLS trust-manager decision logic is untested**
  (`comm/src/transport/hostname_trust_manager.rs:59-172`). Only cert-*generation* consistency is
  tested; wrong-CN and unknown-client-cert rejection are not. *Seam:* the `verify_server_cert`/
  `verify_client_cert` fns + `generate_certificate_if_absent`.

- **G11 — No socket-level transport test exists anywhere.** Every `comm` test is a pure-function
  unit test; no gRPC/TLS handshake or message round-trip over real I/O. *Seam:* a loopback tonic
  server+client (precedent: `node/src/api/grpc/tonic.rs:968` `serves_and_answers_propose`).

## Cross-links

- [`AUDIT.md`](AUDIT.md) — the code findings register (the security fixes the tests must pin).
- [`RUST-FIRST.md`](RUST-FIRST.md) — the native system-contract state model (G3/G5/G6 touch it).
- [`RHO-CALCULUS.md`](RHO-CALCULUS.md) / [`INVENTORY.md`](INVENTORY.md) — the 19-law oracle the
  property + replay tests assert.

## Remediation status

| Gap | Status |
|---|---|
| G1 equivocation | ✅ regression test (`casper/dag.rs`) |
| G2 DoS limits | ✅ RateLimiter + chunker + deploy-pool cap (R2) + decompression cap (R3) + parser depth guard (R9); ⏸ dispatch-semaphore (over tokio's tested primitive) |
| G3 PoS mutations | ✅ `slash`/`pre_charge`/vault deposit/transfer |
| G4 gas enforcement | ✅ `ChargingRSpace` + `phlo_limit` exhaustion end-to-end |
| G5 state-sync | ✅ export→validate round-trip; ⏸ full store export→import→compare |
| G6 history checkpoint/reset/rollback | ✅ `RSpace::create_checkpoint`/`reset`/`revert` |
| G7 replay | ✅ persistent+peek replay matches play; ⏸ `check_replay_data` negative path |
| G8 finalizer | ✅ `calculate_finalization` fork/lockstep |
| G9 malformed input | ✅ `NodeIdentifier`/`KeySegment` + `BlockHash::try_from`/`try_from_hex` (R12) + deploy-signature verify (R1) |
| G10 TLS trust-manager | ✅ wrong-hostname + stale-cert rejection |
| G11 transport socket | ✅ loopback mutual-TLS gRPC send round-trip (`grpc_transport.rs`) |

*(✅ = covered; ⏸ = deferred with the seam documented above.)*
