# Adversarial audit of the Rust port — findings register

This register records the results of the adversarial audit of the Rust port (per the type-system
commitment in [`TYPE-SYSTEM.md`](TYPE-SYSTEM.md) and the invariant catalog in
[`INVENTORY.md`](INVENTORY.md)). It is the durable record of **what was found, what was fixed, what
was assessed faithful, and what remains**, including every deliberate deviation from the Scala
oracle.

The companion page [`RUST-VS-SCALA.md`](RUST-VS-SCALA.md) explains how the Rust rewrite made these
fragile patterns explicit and why it surpasses the Scala original for production readiness.

Audit dimensions (in the order applied):

1. **Type-system conformance — rules** (partiality, casting, raw bytes, untyped numbers) — machine-gated.
2. **Type-system conformance — spirit** (invariants carried structurally; no type escape).
3. **ρ-calculus mirroring** (internals reflect `Name = @Proc`, `Proc = *Name | …`).
4. **Red-team** (exploits, fragile patterns, DoS) — hardening allowed, each Scala deviation documented.

---

## 1. The machine gate

`tools/audit-type-system.sh` is the authoritative, re-runnable gate. It strips `#[cfg(test)]` blocks
(brace-depth aware), then **fails** (exit 1) on:

- **`panic`** — production `.unwrap()` / `.expect(` / `panic!` / `unreachable!` / `todo!` /
  `unimplemented!`, whitelisting `sdk/src/primitive.rs` (the Scala `getUnsafe` escape hatch); the
  rholang parser's `expect(Tok::…)` method is excluded (a method, not `Result::expect`).
- **`unsafe`** — `unsafe {` (must be zero; the crate graph is entirely safe Rust).
- **`silent`** — `try_into().unwrap()` / `try_into().expect(`, and `unwrap_or(0)` /
  `unwrap_or_default()` on a fallible numeric conversion (a fallible conversion must not be
  silently flattened to 0/Default).

Its `cast`/`lax`/`get` classes are candidate finders (soft reports). Baseline (post Phase-0 widen,
`cast` now includes `as i64`, and a new `lax` class catches `from_str_radix(..).unwrap_or(..)` +
`base16::unsafe_decode`): **`panic`/`unsafe`/`silent` clean**; `cast` = 284 candidates, `lax` = 21,
`get` = 79; `cargo clippy` casting lints (`--all-targets`) = 263 `cast_possible_truncation`, 51
`cast_sign_loss`, 26 `cast_precision_loss`, 49 `cast_lossless`. The remediation targets (all 284 cast
+ 21 lax + raw-byte/newtype bypasses) are the checklist in the ρ-pure remediation plan.

---

## 2. Type-system findings — fixed (genuine violations)

| # | Site | Violation | Fix |
|---|---|---|---|
| 1 | `block-storage/src/dag/{finalizer,message_state,message_map}.rs` + `casper/src/dag.rs` | DAG `Message { height: i64, sender_seq: i64 }` bypassed `BlockHeight`/`SeqNum`; casper discharged its (correct) newtypes back to raw `i64` | `Message.height`/`sender_seq` are now `BlockHeight`/`SeqNum`; `message_from_block_metadata` no longer discharges (`casper/src/dag.rs`); `fringe_height` returns `Option<BlockHeight>` (the old `-1` sentinel) |
| 2 | `shared/src/refined.rs` | `Add<i64> for BlockHeight`/`SeqNum` returned `BlockHeight(self.0 + rhs)` — `zero() + (-1)` silently produced a negative height | `Add<NonNegI64>` (the delta carries non-negativity); added `NonNegI64::one()`; call sites use `+ NonNegI64::one()` |
| 3 | `rspace/src/history/radix_tree.rs` | `type Node = Vec<Item>` with `NUM_ITEMS = 256` — the "exactly 256 slots" invariant implicit in a `Vec`; a short/corrupt node panicked on indexing | `Node = [Item; NUM_ITEMS]` (fixed array); `empty_node` = `std::array::from_fn` |
| 4 | `casper/src/runtime_manager.rs:190` | `u64::try_from(cost.value).unwrap_or(0)` — a negative gas cost silently coerced to 0 | reject negative cost (`map_err`); `PCost.cost` is a `uint64`, so a negative (over-charged) cost is an accounting anomaly |
| 11 | `comm/src/transport/chunker.rs` | `max_message_size - 2048` could underflow (wrap) | `checked_sub` returning `Err` on a too-small max size |

**No type escape** — verified: the refined newtypes (`BlockHeight`, `SeqNum`, `Port`, `Hash32`,
`WireLen`, `NonNegI64`) have no `Deref` impl, no public `.get()`/`.value()`
accessor, and no `.0` field access outside `shared/src/refined.rs`.

---

## 3. Type-system findings — assessed **faithful** (not fixed)

These `as` casts are the Rust equivalent of Scala's fixed-width `Int`/`Long`/`Byte` semantics. The
overflow/truncation cases are unreachable in practice (a `Par` cannot have > 2³¹ fields; a deploy
list cannot exceed 255; config durations/sizes cannot exceed `Long` range). Changing them would
**deviate from the Scala oracle**, so they are documented rather than "fixed".

| Site | Cast | Scala oracle | Assessment |
|---|---|---|---|
| `rholang/matcher/par_count.rs` + `par_spatial_matcher_utils.rs` | `par.sends.len() as i32` | `ParCount` fields are Scala `Int` | faithful |
| `casper/{runtime_replay,runtime_manager,block_creator}.rs` | `i as u8` / `(len + i) as u8` into `split_byte` | `Blake2b512Random.splitByte(Byte)` truncates `Int`→`Byte` | **superseded** — subsequently fixed to checked `u8::try_from` (see §8) |
| `node/configuration/{config_mapper,hocon}.rs`, `node/diagnostics/model.rs` | `as_nanos() as i64`, `(n * mult) as i64`, `as f64 … as i64` | Scala `Long` nanoseconds / `Long` byte counts | faithful |
| `crypto/util/sorting.rs` | `(*x as i8).cmp(&(*y as i8))` | Scala `Ordering.by(Array[Byte].toIterable)` orders **signed** `Byte` | **correct** (doc comment already states this) |

---

## 4. ρ-calculus mirroring

- **Fixed** — `rholang/src/matcher/spatial_matcher.rs:681`: the bipartite-match hook
  `spatial_match_fn(…).ok()?.into_iter().next()` silently swallowed an internal `RholangError`
  (e.g. a Law-5 `BugFoundError`) as "no match". Now the error is recorded and propagated when the
  bipartite search finds no matching.
- **Documented (sanctioned design)** — the flat `Par` ADT **erases** the quote `@`/eval `*`
  distinction (`rholang/src/normalizer.rs:131-145,166-178`); the Name/Proc sort is recovered
  structurally by `classify`/`is_pure_name` (`models/src/types.rs`), per `TYPE-SYSTEM.md` §1.1 and
  the Lean `Par.lean` flat record.
- **Stubbed semantics** (honest inventory for the formal spec): set difference `--`
  (`rholang/src/reduce.rs:516-518`); normalizer `defer(...)` cases — `process` dispatch, `complex
  input source`, `concurrent let` (`normalizer.rs`); `substituteAndCharge`/`Chargeable` deferrals
  (`substitute.rs:5`, `accounting.rs:5`, `storage.rs:88`).
- **Deliberate Scala deviations (determinism):** `New.injections` sorted by key
  (`models/src/sorter.rs:324-327`); `locally_free` excluded from equality/hash via `AlwaysEqual`
  (`models/src/ast.rs:35-77`).

---

## 5. Red-team findings

Severity order; all findings are now **Fixed** (or assessed faithful and documented) — no open
red-team items remain.

### Critical

- **C1 — unauthenticated arbitrary-rholang Repl on `0.0.0.0`.** **Fixed.** The gRPC server is split
  into an **external** (deploy, `40401`) and an **internal** (propose + repl, `40402`) listener; the
  internal listener binds `127.0.0.1` (documented deviation from Scala's `0.0.0.0`). The Repl
  `eval` now enforces a phlo limit (`REPL_PHLO_LIMIT = 1e9`, the reducer aborts with
  `OutOfPhlogistonsError` when exhausted) and a wall-clock deadline (`REPL_EVAL_TIMEOUT = 60 s`).
- **C2 — transport `stream` buffered all chunks before the size breaker.** **Fixed:**
  `grpc_transport_receiver.rs::stream` now enforces `max_stream_message_size` *while* draining.
- **C3 — `send` spawned a task per inbound message, unbounded.** **Fixed:** a `Semaphore`
  (`MAX_CONCURRENT_DISPATCH = 1024`) bounds in-flight dispatches; an exhausted semaphore returns
  `ResourceExhausted`.

### High

- **H1/H2 — unauthenticated propose + deploy flooding (autopropose amplification).** **Fixed.**
  Propose now lives on the loopback-only internal server (H1). The external deploy server applies a
  fixed-window rate limit (`DEFAULT_API_RATE_LIMIT_PER_SEC = 100` req/s, H2) via a tonic
  interceptor; excess requests return `ResourceExhausted`.
- **H3 — global `connections` write-lock held across outbound `send`.** **Fixed.**
  `handle_messages::handle`/`handle_protocol_handshake` now take the `RwLock<Vec<PeerNode>>` and hold
  the write lock only for the brief mutation; the handshake `send` runs *before* the lock is taken,
  so a slow peer can no longer stall `/status` or peer dispatch.
- **H4 — block-request bandwidth amplification.** **Fixed.** `PeerRateLimiter`
  (`DEFAULT_BLOCK_REQUEST_LIMIT_PER_SEC = 100` per peer) throttles `handle_block_request`; excess
  requests are dropped with a log.
- **H5 — `BlockRetriever.requested` map unbounded.** **Fixed.** `MAX_REQUESTED_BLOCKS = 10_000` caps
  the map (new hashes are rejected with `AdmitHashStatus::CapacityReached`), and
  `MAX_WAITING_LIST_PER_HASH = 32` caps the per-hash waiting list.
- **H6 — DAG message-state whole-map clone per insert.** **Fixed (partial).** `Finalizer` now
  borrows the message map (`Finalizer<'a>` holds `&'a BTreeMap`) instead of cloning it on every
  `create_message`; both call sites (`message_state.rs`, `multi_parent_casper.rs`) pass a reference.
  The per-message `seen` reachability cache remains an inherent O(N²) structure (faithful to Scala's
  `seen` cache; capping it would break finalization), documented rather than "fixed".

### Medium

- **M2 — `assert!`/`assert_eq!` in the block-receiver state machine.** **Fixed.**
  `end_stored`/`finished` return `Result<…, String>`; the call sites log the error and skip the block
  instead of panicking a spawned task.
- **M4 — no outbound send timeout; `DEFAULT_SEND_TIMEOUT` was dead.** **Fixed:** unary `send` now
  wraps `tokio::time::timeout(DEFAULT_SEND_TIMEOUT, …)`.
- **M3 — blocking LMDB I/O inside async handlers.** **Fixed.** `KeyValueTypedStoreCodec` offloads
  every store op (`get`/`put`/`delete`/`contains`/`to_map`) to `tokio::task::spawn_blocking` (via
  `blocking_lock`), so fsync'd LMDB transactions no longer run on async worker threads. The
  `rchain-shared` `tokio` feature now enables `rt`.
- **M1 — serialized TLS accept.** **Fixed.** The transport receiver now accepts connections in a
  tight loop and spawns each handshake (bounded `MAX_CONCURRENT_HANDSHAKES = 128`), feeding accepted
  streams through a bounded channel; a stalled handshake no longer serializes inbound connections.
  The `0.0.0.0` bind is *faithful* to Scala (the `protocol-server.host` config is the advertised
  address, not the bind address).
- **M5 — peer-table fillability.** **Fixed.** `update_last_seen` evicts the least-recently-seen
  entry when a bucket is full and every entry is already pending a ping, so a full bucket can never
  saturate permanently.
- **M6 — unauth `/reporting/trace` forceReplay.** **Fixed.** `reporting_trace` returns `404` unless
  `api-server.enable-reporting` is set (the flag is now threaded through `HttpState`).
- **M7 — plaintext-HTTP external-IP discovery.** **Assessed faithful** — Scala uses the same
  plaintext `http://` endpoints (`WhoAmI.scala`). Switching to `https://` requires a TLS-client
  dependency; documented as a low-risk limitation.
- **M8 — bootstrap retry-forever.** **Fixed.** `keep_on_requesting_till_running` gives up after
  `MAX_BOOTSTRAP_RETRIES = 10` attempts, so a dead bootstrap no longer blocks node startup.

**Crypto** is defensive: signature-verify and key parsing return `false`/`Err` on malformed input.
Noted low-severity: `PBKDF2_ITERATIONS = 1024` (`crypto/util/key_util.rs:24`, local-only).

---

## 6. Scala-deviation register

Every place the Rust port deliberately departs from the Scala oracle, with the reason.

| Deviation | Oracle location | Reason |
|---|---|---|
| `New.injections` sorted by key (determinism) | `models/.../rholang/*` | `HashMap` order is non-deterministic in Rust |
| `locally_free` excluded from `Eq`/`Hash` (`AlwaysEqual`) | `models/.../Par.scala` | the cache field is not part of structural identity |
| Negative deploy cost **rejected** (not wrapped to `uint64`) | `accounting/Costs.scala` `toProto` = `PCost(c.value)` | Scala wraps a negative `Long` into `uint64` (latent bug); reject is safer |
| `Add<NonNegI64>` for heights (no negative delta) | — | invariant preserved structurally |
| gRPC `max_decoding_message_size` wired (was 4 MB tonic default) | `defaults.conf` `grpc-max-recv-message-size = 16M` | honors the existing config |
| transport `send` timeout (`DEFAULT_SEND_TIMEOUT`) | `GrpcTransportClient.DefaultSendTimeout` | the constant existed but was unused |
| stream size cap enforced while draining | `StreamHandler.collect` | Scala checks the cap during the fold; the port had moved it after full buffering |
| semaphore-bounded inbound dispatch | per-peer `LimitedBufferObservable` | bounded-queue analog |
| super-majority as exact integer `3·stake > 2·total` | `sdk/consensus/Stake.scala` `stake.toDouble / totalStake > 2d/3` | Law 14 is "strictly > 2/3"; the f64 form loses precision for stakes ≥ 2⁵³ (recorded in §2 of the ρ-pure remediation) |
| `bonds_map`/stake carried as `NonNegI64` (reject negative) | `Message.bondsMap`/`BlockMetadata.bondsMap`/`BlockMessage.bonds` are `Long` in Scala | stake is non-negative by the PoS invariant; negative stakes are rejected at the proto/genesis boundary rather than silently carried as signed `i64` |
| internal gRPC (propose + repl) binds `127.0.0.1` | Scala binds both servers to `0.0.0.0` | unauthenticated propose/repl are no longer network-reachable (C1/H1) |
| external/internal gRPC split (deploy `40401`, propose+repl `40402`) | Scala has the same split (`port-grpc-external`/`internal`) | the port previously put all three services on one listener |
| Repl phlo limit + wall-clock deadline | Scala runs Repl with no limit | a runaway term must not drain the node (C1) |
| deploy gRPC rate limit (100 req/s) | Scala has no limit | bound unauthenticated deploy flooding (H2) |
| per-peer block-request rate limit (100/s) | Scala serves every request | bound block-request bandwidth amplification (H4) |
| `BlockRetriever.requested` capped (10k) + waiting-list capped (32) | Scala map is unbounded | bound peer-advertised hash flooding (H5) |
| `Finalizer` borrows the message map (no clone) | Scala clones the map per call | remove the O(map) clone per message (H6) |
| `connections` write-lock released before outbound I/O | Scala holds the `Ref` across the send | a slow peer must not stall the connection table (H3) |
| concurrent (bounded) TLS handshake accept | Scala serializes accepts on the handshake | a stalled handshake must not stall inbound connections (M1) |
| store ops offloaded to `spawn_blocking` | Scala runs LMDB on the effect runtime | fsync'd LMDB writes must not block async workers (M3) |
| peer-table evicts least-recently-seen when saturated | Scala drops the peer (relies on the ping RPC) | without a ping RPC a full bucket would saturate permanently (M5) |
| `/reporting/trace` gated on `enable-reporting` | Scala reads the flag but does not enforce it | the flag must actually gate the route (M6) |
| bootstrap request gives up after 10 retries | Scala `keepOnRequestingTillRunning` retries forever | a dead bootstrap must not block startup (M8) |
| deploy pool capped (`MAX_POOLED_DEPLOYS = 10_000`) | Scala deploy pool is unbounded | a remote flood must not exhaust the deploy store (R2) |
| stream decompression capped (`content_length ≤ max_stream_message_size`) | Scala `LZ4Compressor` does not bound decompressed size | reject a decompression bomb before allocating (R3) |
| Kademlia RPC rate-limited (100 req/s) | Scala Kademlia ping/lookup are unlimited | bound sybil/routing-table pollution + peer enumeration (R5) |
| exploratory deploy phlo limit (`1e9`) + 60 s deadline | Scala runs exploratory deploy with no limit | a runaway term must not drain a read-only node (R6) |
| private keys written owner-only (`0o600`) | Scala `fs.write` uses default perms | secret material must not be world-readable (R8) |
| rholang parser depth guard (`MAX_PARSE_DEPTH = 512`) | Scala BNFC parser has no depth guard | a deeply-nested term must not overflow the stack (R9) |
| HTTP `/api/deploy` + explore routes rate-limited (100 req/s) | Scala HTTP deploy routes are unlimited | match the gRPC deploy rate limit (R10) |
| PBKDF2 iterations raised `1024 → 310_000` | Scala uses BouncyCastle default `1024` | slow offline brute-force of encrypted keys at rest (R11) |

---

## 7. Verification

- `cargo check --workspace` — clean.
- `cargo test --workspace --exclude rchain-crypto` — all green (crypto has a pre-existing flaky
  `read_key_pair_round_trips_private_key`).
- `tools/audit-type-system.sh` — zero hard production violations (`panic`/`unsafe`/`silent`).
- New tests: `arithmetic_preserves_non_negativity` (`refined.rs`); existing radix-tree / message-state
  tests cover the array-node and `BlockHeight`/`SeqNum` refactors.

---

## 8. ρ-pure remediation (post prime-directive change)

Under the new oracle (the ρ-calculus spec, not Scala), the following were **fixed**:

- **Consensus super-majority** — `sdk/src/consensus.rs` f64 → exact integer `3·stake > 2·total`
  (Law 14 precision loss for stakes ≥ 2⁵³).
- **Stake → `NonNegI64`** — `Message`/`BlockMetadata`/`BlockMessage`/`compute_bonds`/
  `fringe_bonds_map`/`unsigned_block_proto`/`bonds_parser`/`contracts.Validator.stake`; negative
  stake rejected at the proto/genesis/bonds-file boundary.
- **`split_byte` seed** — `i as u8`/`(len+i) as u8`/`i as u16` → checked `u8::try_from`/`u16::try_from`
  (replay/proposer/reduce), so an oversized deploy list errors rather than wrapping the seed.
- **`BlockData` height/seq** — `BlockHeight`/`SeqNum` carried through; discharge only at the
  `rho:block:data` contract boundary.
- **Raw-byte escapes** — `PublicKey` field closed (private), `KeySegment` `TryFrom<Vec<u8>>` (≤127),
  `NodeIdentifier::from_hex` rejects odd-length/non-hex, `base16::try_decode` added and used at the
  `is_finalized` API boundary.
- **Signed-byte ordering** — `cmp_signed_byte` helper in `crypto::util::sorting` (shared with the sorter).
- **Rholang parser completed** — map-vs-block disambiguation (`{k:v}` was misparsed as a braced
  process), `_` wildcard lexing, `bundle0` → `bundle`+`0`, and multi-receipt `for` desugaring
  (`for (r1; r2; …) { P }` → nested receives). The 9 blessed genesis contracts now parse.
- **Genesis boot fixed** — the deploy normalizer env now binds `rho:rchain:deployerId`/`deployId`
  (`NormalizerEnv::new(deploy)`; it was empty, so the URI was unbound at `eval_new`); the
  `tokio::spawn(node_launch)` result is logged instead of dropped; `create_block_with_processed_deploys`
  `assert!` → `Result`; the runtime uses a 32 MiB worker stack (the blessed terms recurse past the
  2 MiB default).

**Assessed (not fixed — unreachable / boundary / over-engineering):**

- **Length prefixes** (`radix_tree` `& 0x7F`, `certificate_helper` DER, `merging`/`block_random_seed`
  varints+`uint16`, `state/mod`, `scodec`): the lengths are bounded by the wire format (`KeySegment`
  ≤127), the protocol (32-byte hash / 65-byte key), or gas. The `& 0x7F` mask is the 7-bit
  flag+length wire format, not a bug.
- **Config/diagnostics casts** (`as_nanos() as i64`, `(n*mult) as i64`, f64→i64): faithful to Scala
  `Long` nanoseconds/bytes; the truncation needs > 292-year durations or > 2⁶³-byte sizes.
- **API heights** (`block_api_impl` `i32`/`i64` query params, `latest_block_number() -> i64`): the
  `i32` query depth and the potentially-negative lower bound are legitimate API types; `m.height` is
  already `BlockHeight` with discharge at the DTO.
- **DTO boundary** (`deploy_service.rs`, `node/src/api/dto.rs`, `web/*`): `String`/`Vec<u8>` at the
  wire edge is acceptable; validate-on-ingress remains a follow-up (noted, not done).
- **`rho_expr.rs` `unsafe_decode`**: converting `rho_expr_to_par`/`unforg_to_par` to `Result` is a
  follow-up (the recursive `.map` would become `try_collect`).

**Cast triage (Phase 2):** the ~300 `cast` sites were triaged. The overwhelming majority are **faithful
Scala fixed-width equivalents** — matcher `len() as i32` (Scala `Int`), trie `byte as usize`/`u8 as usize`
(widening), `i as u8` `split_byte` (Scala `Byte`), config `as i64`/`as u64`/`as f64` (Scala `Long`),
crypto `*x as i8` (Scala signed `Byte`), diagnostics `as f64` (display), wire-format varint/zigzag.
The consensus-critical path is already exact (`NonNegI64` stakes, `i128` super-majority). Two genuinely
untrusted-input narrowing casts were **fixed**: state-sync `skip`/`take` (`node_running.rs` — negative
`i32` no longer wraps to a huge `usize`) and the store-node-key index (`store_node_key_from_proto` —
out-of-range `i32` no longer truncates via `as u8`). The remaining casts are bounded by the protocol
(32-byte hash / 65-byte key), gas, or block count.

---

## 9. Rust-first reimplementation — fragility audit of the Scala-port rholang layer

This section is the justification for the rust-first reimplementation (plan
`delegated-crafting-phoenix.md`; plans live outside the repo, in the project's `~/.claude/plans/`
directory). It documents *why* the current rholang layer is fragile — not as a
list of bugs to patch, but as the record of the Scala legacy the reimplementation removes. For each
finding: **what it is → why it is fragile/exploitable → how the rust-first rewrite eliminates it**.
Scala remains a *checklist* of required behavior only, never an implementation guide.

- **F1 — The interpreter core is a mechanical Scala port.** `rholang/src/reduce.rs` (1773 lines)
  mirrors `Reduce.scala`/`DebruijnInterpreter`; `normalizer.rs` (1807 lines) mirrors the
  BNFC-derived compiler; `matcher/*` ports the cats-effect `StateT`/`StreamT` monad stack to concrete
  `Vec<FreeMap>` backtracking. *Why fragile:* the effect stack is shoe-horned into `async` +
  concrete collections, and the structural invariants (`locally_free`, `connective_used`) are
  maintained **by hand** (`reduce.rs:35-59`, `substitute.rs`) rather than carried by the type. A
  single inversion (the `normalize_contr` formal-order reversal, fixed in `5ae8dc4df`) silently broke
  list-as-channel matching — latent bugs are invisible until one contract exercises them. *How the
  rewrite eliminates it:* the interpreter is re-derived from the 19 laws (`INVENTORY.md`) and the
  grammar in `RHO-CALCULUS.md`, with the `Par<S>` sort split and the `Closed`/`WellScoped`/
  `BindsAtMostOnce` refinements carrying the invariants structurally (Phase 4).

- **F2 — The blessed genesis contracts re-implement a HashMap trie in interpreted rholang.**
  `casper/src/genesis/resources/Registry.rho:80-368` is a depth-4 keccak-256 nybble trie with 16-bit
  power-of-two bitmasks, built on `@[node, *storeToken]` list-channels and `@(map, "depth")`
  tuple-channels. *Why fragile:* it stresses every exotic interpreter feature at once — peek `<<-`,
  persistent `!!`, list/tuple channels, method calls, keccak trie hashing, bitmask arithmetic — and
  any one of them failing makes the registry **silently empty** (the `process_deploy` path only
  surfaces *reported* errors, `runtime_manager.rs:198`). *How the rewrite eliminates it:* the
  registry/PoS/vault become **native Rust system processes** over a native `BTreeMap` state, exposed
  on the same `rho:*` protocol but with no rholang trie to execute (Phases 1–3).

- **F3 — Silent partiality hides the failure.** `compute_bonds` (`casper/src/runtime_manager.rs:503-509`)
  runs an exploratory deploy (`BONDS_QUERY_SOURCE`, `:547-552`); a non-matching receive yields 0
  results → an empty bond map → "Incorrect number of results: 0", with no indication *which* link
  broke (registry lookup? PoS `getBonds`? the `for(@(_, Pos) <- poSCh)` pattern?). *Why fragile:*
  three separate reductions must all succeed for a single fact (the bonds map) to be observable, and
  the failure mode is a count mismatch rather than a typed error. *How the rewrite eliminates it:*
  `compute_bonds` becomes a single native read (`HistoryReader::get_native(PREFIX_POS, …)`), total and
  typed (Phase 2).

- **F4 — Gas metering is unwired.** `ChargingRSpace` (`rholang/src/storage.rs:88`) is a pure
  passthrough (storage/event charging deferred); `substituteAndCharge` (`substitute.rs:5`) and the
  proto-size cost table (`accounting.rs:5`) are deferred; `Chargeable` has no instances. *Why
  fragile:* the node's primary DoS defense (phlo) is not actually enforced against untrusted deploy
  work. *How the rewrite eliminates it:* `substituteAndCharge`, the proto-size `Costs`, `Chargeable`,
  and `ChargingRSpace` storage/event charging are implemented (Phase 4.7).

- **F5 — Scala-specific encodings leaked into the port.** The CRC14 + 270-bit `ZBase32` registry URI
  (a Scala-specific `org.lightningj.util` dependency) was carried into the port before being dropped
  for the rust-first `rho:id:` + z-base-32 encoding (`rholang/src/registry.rs`); the `.rho`/`.rhox`
  headers still document the stale 55-char Scala URIs. *Why fragile:* non-spec, non-reproducible
  encodings coupling the state hash to a JVM library. *How the rewrite eliminates it:* the URI
  derivation is spec-driven and self-consistent, and the blessed contracts are demoted to checklist
  fixtures (Phase 3).

**Status:** the reimplementation is tracked by the plan `delegated-crafting-phoenix.md` (plans live
outside the repo, in `~/.claude/plans/`); each phase closes the corresponding finding (F1→Phase 4,
F2/F3→Phases 1–3, F4→Phase 4.7, F5→Phase 3).

---

## 10. Full-system HAZOP

A guideword analysis over the whole crate graph. **Guidewords** (domain-adapted): **No/Not** (missing
check/field), **More** (unbounded / overflow / amplification), **Less** (truncation / underflow /
negative), **As well as** (extra unvalidated input), **Part of** (partial data), **Reverse** (ordering
/ sign flip), **Other than** (wrong identity / type / field), **Early/Late** (TOCTOU / expiry),
**Before/After** (state ordering / race). Each Safeguard references the finding it closes (R1–R12, or
the prior C1–C3/H1–H6/M1–M8 register).

| Node | Parameter | Guideword | Consequence | Safeguard |
|---|---|---|---|---|
| N1 Transport | `content_length` (decompressed) | More | i32::MAX allocation → OOM | cap ≤ `max_stream_message_size` (R3) |
| N1 Transport | compressed stream bytes | More | unbounded buffering | cap enforced while draining (C2) |
| N1 Transport | inbound dispatch concurrency | More | unbounded tasks | semaphore 1024 (C3) |
| N1 Transport | TLS handshake concurrency | More | serialized accept | bounded 128 (M1) |
| N1 Transport | peer send | Late | no timeout | 5 s `send` timeout (M4) |
| N1 Transport | key file mode | Other than | world-readable key | `0o600` write (R8) |
| N2 Deploy | deploy signature | No | unsigned deploy accepted on gRPC | verify at ingress (R1) |
| N2 Deploy | deploy pool size | More | disk exhaustion | cap 10k (R2) |
| N2 Deploy | `phlo_limit × phlo_price` | More | i64 wrap → gas bypass | `checked_mul` (R4) |
| N2 Deploy | `phlo_limit` sign | Less | negative limit | reject at ingress (R4) |
| N2 Deploy | HTTP deploy rate | More | flood | HTTP limiter (R10) |
| N3 Consensus | `attestation_stake`/`total_stake` sum | More | i64 overflow → false majority | accumulate in i128 (R7) |
| N3 Consensus | super-majority comparison | More | f64 precision loss | exact `3·stake > 2·total` (Law 14) |
| N3 Consensus | equivocation (`seq_num` reuse) | As well as | double block by a sender | rejected before write (H-1) |
| N4 Crypto | PBKDF2 iterations | Less | fast offline brute-force | 310 000 (R11) |
| N4 Crypto | signature/key parse | Other than | panic on malformed input | `false`/`Err` (defensive) |
| N5 State/replay | LMDB I/O in async | Before/After | blocked workers | `spawn_blocking` (M3) |
| N6 Parser | parse nesting | More | stack exhaustion | `MAX_PARSE_DEPTH` (R9) |
| N6 Parser | `from_slice` length | Other than | panic on wire input | `TryFrom<&[u8]>` (R12) |
| N6 Parser | hex decode | As well as | lax non-hex decode | `try_decode` at ingress (R12) |
| N7 Discovery | ping/lookup rate | More | table pollution / sybil | rate limit 100/s (R5) |
| N7 Discovery | routing `(id, host, port)` | Other than | arbitrary outbound conn | enforced at mTLS handshake (mTLS) |
| N8 RSpace | radix node slots | Less | short node panic | fixed `[Item; 256]` (finding 3) |
| N9 Cost/gas | exploratory deploy | More | unbounded phlo/time | phlo cap + deadline (R6) |
| N9 Cost/gas | gas metering | No | phlo not enforced | charging implemented (F4) |

---

## 11. Red-team re-audit findings (pass 2)

A fresh attacker's-eye pass over the network surface (this session). Severity order; **all fixed** in
this pass. Each entry: **site → root cause → fix → classification** (pure bug fix vs documented Scala
deviation) → verification.

### Critical (P0)

- **R1 — deploy signature not verified on the gRPC path.** `casper/src/api/block_api_impl.rs::deploy`
  checked read-only/shard/forbidden-key/phlo-price but never the signature; the HTTP path verifies
  (`node/src/api/conversion.rs::to_signed_deploy` → `Signed::from_signed_data`). **Fix:**
  `SignedDeployData::verify_signature` (`models/src/casper/protocol/casper_message.rs`) recomputes the
  same hash/verify as the HTTP path, and `BlockApiImpl::deploy` rejects an invalid signature first.
  **Pure bug fix.** Verified: `verify_signature_accepts_valid_deploy` / `_rejects_tampered_term` /
  `_rejects_unknown_algorithm`.
- **R2 — unbounded deploy pool.** `casper/src/dag.rs::add_deploy` put without bound (keyed by
  signature). **Fix:** `MAX_POOLED_DEPLOYS = 10_000` cap via a new `count()` on `KeyValueTypedStore`
  (O(1) `num_records` on the byte store). **Documented deviation.** Verified:
  `add_deploy_rejects_when_pool_full`.
- **R3 — lz4 decompression bomb.** `comm/src/transport/stream_handler.rs::decompress_content`
  allocated attacker-controlled `content_length` before `lz4_flex::block::decompress`; the receiver
  capped only compressed bytes. **Fix:** thread `max_decompressed_size` into `restore`/`decompress_content`
  and reject before allocating (`grpc_transport_receiver.rs` passes `max_stream_message_size`).
  **Documented deviation.** Verified: `restore_rejects_oversized_decompressed_content`.
- **R4 — unchecked phlo multiply.** `models/.../casper_message.rs::total_phlo_charge` did
  `phlo_limit * phlo_price` in i64 (wrap/panic); `runtime_replay.rs::refund_amount` repeated it.
  **Fix:** `Option<i64>` via `i128::checked_mul`; propagate `Err` at the two pre-charge sites; clamp
  the refund; reject negative `phlo_limit` at ingress. **Pure bug fix.** Verified:
  `total_phlo_charge_does_not_wrap_on_overflow`.

### High (P1)

- **R5 — plaintext unauthenticated Kademlia discovery on `0.0.0.0:40404`.** Only a non-secret
  `network_id` gates ping/lookup; peers inject arbitrary `(id, host, port)`. **Fix:** rate-limit the
  RPC (`DEFAULT_KADEMLIA_RATE_LIMIT_PER_SEC = 100`) via the shared `RateLimiter`. The `0.0.0.0` bind
  is kept (faithful to Scala and to the transport's own bind convention; the discovered external IP is
  not a local bind address, and the routing table only affects peer discovery, not consensus safety).
  **Documented deviation** with the residual "plaintext + non-secret network_id" risk noted.
- **R6 — unbounded `exploratory_deploy`.** `casper/src/runtime_manager.rs::capture_results` ran
  `evaluate` with no phlo/timeout (reachable on public read-only nodes). **Fix:** mirror the Repl
  bound — `EXPLORATORY_PHLO_LIMIT = 1e9` + `EXPLORATORY_EVAL_TIMEOUT = 60 s`. **Documented deviation.**
- **R7 — i64 stake-sum overflow.** `casper/.../proposer.rs` and `block-storage/.../finalizer.rs`
  summed `NonNegI64` stakes into `i64` before the super-majority comparison. **Fix:** accumulate in
  `i128`; widen `sdk/src/consensus.rs::is_super_majority` to `(i128, i128)`. **Pure bug fix.**
  Verified: `i64_overflowing_stakes_do_not_wrap`.

### Medium (P2)

- **R8 — private keys/certs written with default perms.** `generate_certificate_if_absent.rs`,
  `bonds_parser.rs`, `key_util.rs` used `fs::write` (default `0666 & ~umask`) for secret material.
  **Fix:** `crypto::util::key_util::write_private_key` (owner-only `0o600`) at all three sites.
  **Documented deviation.**
- **R9 — rholang parser has no recursion-depth guard.** `rholang/src/parser.rs` recursed without bound
  (≤16 MB terms via gRPC). **Fix:** `MAX_PARSE_DEPTH = 128` + `Parser::with_depth` wrapping the
  recursive-descent roots (`parse_proc`, `parse_proc1`, `parse_proc10`, `parse_proc15`, `parse_name`).
  **Documented deviation.** Verified: `rejects_excessive_nesting_depth`.
- **R10 — HTTP `/api/deploy` not rate-limited.** Only the gRPC deploy server was rate-limited.
  **Fix:** shared `RateLimiter` promoted to `rchain_shared`; `HttpState.deploy_rate_limiter` gates
  `api_deploy`/`api_explore_deploy`/`api_explore_deploy_by_block_hash` (`429`). **Documented deviation.**

### Low (P3)

- **R11 — `PBKDF2_ITERATIONS = 1024`.** **Fix:** raised to `310_000` (OWASP PBKDF2-HMAC-SHA256).
  **Documented deviation** (interop caveat: keys written with the new count are not readable by
  BouncyCastle-1024 tooling and vice-versa).
- **R12 — validate-on-ingress `from_slice` asserts.** `models/{block_hash,block/state_hash,validator}.rs`
  `from_slice` panicked on wrong-length wire input (whitelisted in the gate); `BlockHash::from_hex`
  used lax `unsafe_decode`. **Fix:** `TryFrom<&[u8]>` checked constructors (a new `ModelsError::Length`
  variant) + `BlockHash::try_from_hex` (via `base16::try_decode`); the `/reporting/trace` ingress now
  returns 400 instead of panicking. Completed in the follow-up pass: the wire-ingress decoders
  (`BlockMessage`/`BlockMetadata`/`FringeData`/`FinalizedFringe`/`BlockHashMessage`/`StoreItemsMessage*`/
  `SystemDeployData` `from_proto`/`from_bytes`) and the API hex-query sites (`block_api_impl`,
  `deploy_grpc_service_v1`) now use `TryFrom` and return `Result`; `Blake2b256Hash::from_byte_array`
  gained a `TryFrom<&[u8]>` and the state-sync `StoreItemsMessage` path uses it. The panicking
  `from_slice`/`from_byte_array` are now reachable only from internally-produced fixed-width data
  (internal invariants). **Pure bug fix.**

**Verification (this pass):** `cargo check --workspace` clean; `tools/audit-type-system.sh` zero hard
violations (the `expect(Tok::…)` parser exclusion was widened to cover the `with_depth` receiver `p`);
`cargo test` green for `rchain-models`/`rchain-sdk`/`rchain-comm`/`rchain-rholang`/`rchain-casper`/
`rchain-node`; `rchain-crypto` green except the pre-existing flaky
`read_key_pair_round_trips_private_key` (shared temp-dir race, unrelated); `cargo clippy --workspace
--all-targets` clean on the changed files. The parser guard is `MAX_PARSE_DEPTH = 128` (512/256
overflowed the 2 MiB test-thread stack during recursion; 128 is stack-safe and far deeper than any
real rholang term).

