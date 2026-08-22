# Architecture & port status

The port is a Cargo workspace at the repo root — one crate per original sbt module, mirroring the sbt
dependency graph. This chapter consolidates the layer map, per-crate status, rewrite order, and
remaining work (folded from [`AGENTS.md`](../../../AGENTS.md) and the project
[`README.md`](../../../README.md)).

## Layer map

- **Rholang** (`rholang/`, `models/`) — the ρ-calculus interpreter. Key invariants: canonical total
  order (`models/.../rholang/sorter/ScoreTree.scala`), capture-avoiding substitution
  (`rholang/.../interpreter/Substitute.scala`), reduction (`Reduce.scala`), spatial matching
  (`interpreter/matcher/SpatialMatcher.scala`). The K-framework semantics under
  `legacy/rholang/src/main/k/rholang/` are the (unfinished) reference semantics.
- **RSpace** (`rspace/`) — the concurrent tuple space. Key invariants: join commutativity
  (`hashing/StableHashProvider.scala`), deterministic COMM (`trace/Event.scala`), merge monoid
  (`merger/StateChange.scala`, `merger/EventLogMergingLogic.scala`), Merkle radix trie
  (`history/RadixTree.scala`), replay (`ReplayRSpace.scala`).
- **Rosette** (`rosette/`, `roscala/`) — the C++ actor VM. Key invariants: actor atomicity, reflective
  meta/parent chain, fork-join barrier.
- **Casper** (`casper/`, `block-storage/`, `sdk/`) — CBC-Casper consensus + DAG. Key invariants: >2/3
  finality (`sdk/.../consensus/Stake.scala`), fringe/estimator (`block-storage/.../dag/Finalizer.scala`,
  `MessageMapSyntax.scala`), block validation (`casper/.../Validate.scala`), merge determinism
  (`sdk/.../merging/ConflictResolutionLogic.scala`).
- **Crypto** (`crypto/`) — Blake2b256, `Blake2b512Random`, secp256k1, Curve25519.

## Module status

Ported in dependency-respecting order (easiest/leaf modules first). Each crate mirrors one upstream
Scala module (now under [`legacy/`](../../../legacy/)):

| Crate | Status |
|-------|--------|
| `sdk` | **done** (root leaf; Laws 14 & 17; DAG interface: `BlockRequester`/`DagManager`/`DagView`/`DagData` + Casper validation syntax + `FatalError` + primitive syntax) |
| `shared` | **core done** (Base16/Serialize/DagOps/store+KeyValueCache/Stopwatch/LongOps/PathOps/SeqOps/Matcher/Language/Time/Debug helpers + LMDB store under the `lmdb` feature) |
| `crypto` | **done** (Law 19: Blake2b256, Blake2b512Random, secp256k1/Ed25519, Curve25519, PEM key writing) |
| `graphz` | **done** (DOT builder) |
| `models` | **done** (rholang AST + Law 1 sorter + Casper/routing wire layer + JSON serde) |
| `block-storage` | **done** (DAG finalizer + BlockStore/ApprovedStore/BlockDagStorage) |
| `rspace` | **done** (hashing/radix-tree/history/merger + play/replay engine, merger execution `computeTrieActions`, replay verification, reporting, hot-store back-fill, util, state/exporters incl. `traverseHistory`/`validateStateItems` + store-backed instances, store→ReplayRSpace factory, and the disk exporter `RSpaceExporterDisk.writeToDisk` in `state/exporters`) |
| `comm` | **done** (PeerNode/PeerTable + Kademlia gRPC discovery, gRPC/TLS transport client/server/receiver, buffers/PacketOps/StreamHandler, rp Connect/HandleMessages, WhoAmI external-IP discovery + UPnP port-forwarding orchestration + weupnp SSDP/SOAP gateway discovery) |
| `rholang` | **done** (Env + de Bruijn substitution + accounting + spatial matcher + Reduce/dispatch + normalizer/compiler + full parser — map-vs-block disambiguation, `_` wildcard, `bundle+/-/0`, peek `<<-`, method calls, n-ary `par`, multi-receipt `for`; **native system contracts** — `rho:registry:{lookup,insertArbitrary,insertSigned:secp256k1}`, `rho:rchain:pos`, `rho:rchain:revVault`/`multiSigRevVault` as Rust system processes over `native_state.rs` (see [`spec/RUST-FIRST.md`](../../../spec/RUST-FIRST.md)); **gas metering** — `Chargeable` + size-proportional costs + `substituteAndCharge` + `ChargingRSpace` produce/consume storage/event charging; PrettyPrinter/StoragePrinter + RhoRuntime/ReplayRhoRuntime/ReportingRuntime + `par_ops` in `models`) |
| `casper` | **done** (validate effectful checks, RuntimeManager, merge index/merging, BlockApi/BlockApiImpl, BlockReportApi, GraphGenerator, reporting/rhoReporter, multi-parent Casper, genesis (native system-contract install + de-blessed `createGenesisBlock`; `compute_bonds`/`get_active_validators` read the native PoS leaf), protocol/engine/storage, comm/discovery wiring: CommUtil/BlockReceiverState+not_validated/BlockRetriever/NodeRunning handlers, engine state machines: LfsState/LfsTupleSpaceState/LfsBlockRequester/LfsTupleSpaceRequester/NodeSyncing/NodeRunning/BlockReceiver.apply + NodeLaunch genesis helpers) |
| `node` | **done** (configuration, diagnostics, api incl. Deploy/Propose/Repl gRPC adapters + WebApi/AdminWebApi + DTOs, tonic gRPC + axum HTTP `/api` transport binding, NodeRuntime/Setup assembly + `rnode` binary, web routes/status/version/transaction, effects/runtime REPL, dag, instances incl. ProposerInstance, revvaultexport, CLI subcommands (`runCLI` thin-client: deploy/deploy-status/find-deploy/propose/show-block/show-blocks/vdag/mvdag/listen-*/last-finalized/is-finalized/bond-status/status/keygen/repl/eval) backed by tonic gRPC clients, `/reporting` trace route over `BlockReportApi`, `/status` route, `/api/v1` JSON routes, node runtime wiring (`setupNodeProgram`: `RSpaceImporterStore` factory, block receiver/processor streams, transport/protocol server + peer-message stream, `NodeLaunch.apply`, request-dependencies loop, proposer stream + `BlockApiImpl` propose trigger + autopropose tap)); the genesis ceremony boots end-to-end under a 32 MiB worker stack (deep contract recursion), verified by the node-level integration tests (`genesis_boot_exposes_block_over_http`, `http_surface_without_genesis`); the interactive REPL has a rustyline prompt/history/tab-completion + an isolated `eval-*` store; a scripted Docker multi-node pipeline (`docker/rnode/Dockerfile` + `tools/docker-network.sh`) boots a 1–5 node network; the `/api/v1/openapi.json` OpenAPI document route is served |
| `rspace-bench` | **done** (criterion crate: `EvalBench` `mvcepp.rho` reduce, `WideBench` `wide-setup.rho`+`wide.rho`, `KeyBench` `Blake2b256Hash` key encode, and raw play/replay `RSpace` produce/consume) |

Deferred (orphaned, not wired into `build.sbt`): `legacy/roscala/`, `legacy/rosette/` (C++ VM).

## Rewrite order & ratings

The 15 components were scoped for a faithful Rust port and rated by difficulty and time. The
**rewrite order is dependency-driven and easiest-first**; it deliberately differs from the
*formalization* phases (which rank by invariant value — see [`AGENTS.md`](../../../AGENTS.md)). Both run
in parallel: laws are proven in phase order while code is ported in dependency order.

### Ratings (main LOC → difficulty → est. person-days)

| Module | Main LOC | Difficulty | Est. | Depends on | Note |
|---|---|---|---|---|---|
| `graphz` | 231 | Easy | ~1 | `shared` | trivial string builder |
| `sdk` | 678 | Easy | ~3 | `shared` | Laws 14, 17 |
| `crypto` | 1,431 | Easy | ~5–8 | `shared` | 1:1 crate mappings |
| `rspace-bench` | (bench) | Easy | ~3–5 | rspace/rholang/models | gated |
| `block-storage` | 1,074 | Medium | ~7 | shared/models/sdk | finalizer + monotonicity |
| `shared` | 3,092 | Easy–Med | ~10–15 | — | foundational; LMDB FFI |
| `models` | 4,252 | Medium | ~12–18 | shared/crypto | bit-exact sorter |
| `comm` | 3,366 | Hard | ~15 | shared/crypto/models | lock-free buffers, gRPC/TLS |
| `rspace` | 6,840 | Hard | ~20–30 | shared/crypto | concurrency, Merkle, replay |
| `node` | 7,456 | Medium | ~30–45 | casper/comm/crypto/rholang | glue |
| `rholang` | 9,372 | Hard | ~30–45 | models/rspace/shared/crypto | interpreter, gas, matcher |
| `casper` | 9,916 | Hard | ~50 | everything | central hub |
| `roscala` | 4,533 | Hard | ~25–40 | — | **orphaned — defer** |
| `rosette` (C++) | ~50k | Hard | ~80–150 | — | **orphaned — skip** |

Total in-scope (non-orphaned): roughly **200–220 person-days**.

### Rewrite order (bottom-up)

`sdk` → `shared` → `crypto` + `graphz` → `models` → `block-storage` +
`rspace` + `comm` → `rholang` → `casper` → `node` → `rspace-bench`. **Defer** `roscala`/`rosette`.

### Findings

- **`rosette`/`roscala` are orphaned** (absent from `build.sbt`, imported by nothing) — deferred.
- **Hoist `Blake2b256Hash`** out of `rspace` into `crypto`/`shared` so `models` stops depending on
  `rspace` (`models/.../ByteStringSyntax.scala`, `FringeData.scala`, `BlockMetadata.scala`).

## Remaining work

The execution core, RSpace, rholang, casper, and the node's pure/API surface are ported — including
the LMDB store, the node transport binding + `runCLI` (thin-client deploy/propose/status/keygen/
repl/eval/listen over tonic gRPC clients), the comm/discovery engine wiring (`CommUtil`/
`BlockReceiver`/`BlockRetriever`/`NodeRunning`), and the full casper engine (LFS requesters,
`NodeSyncing`, `NodeRunning`, `NodeLaunch.apply`, `BlockReceiver.apply`, `BlockProcessor.apply`).
The node runtime wiring (`setupNodeProgram`) is also ported: the `RSpaceImporterStore` factory, the
block receiver/processor streams, the transport/protocol server + peer-message stream,
`NodeLaunch.apply`, and the request-dependencies loop.
Remaining:

- **Formalization** — Law 1 (canonical order), Law 2 core (`≡`), Law 4 core (`⟶`), and Law 6
  (`Closed`) are **proven** in Lean (`spec/Rchain/`); Laws 3, 4-full, 5, and 7–19 are **stated** with
  precise signatures (`Subst`/`Reduce`/`Match`/`FreeVars`/`RSpace/*`/`Casper/*`/`Crypto/*`), and Law
  19's crypto primitives are axiomatized by design. Law 1's 30 element-comparator `axiom`s remain to discharge
  (blocked on a mutual-recursion refactor). Laws 12–13 (Rosette) are **orphaned** (the VM is out of
  scope). See [`spec/README.md`](../../../spec/README.md) and
  [`spec/INVENTORY.md`](../../../spec/INVENTORY.md).
- **Cast / raw-byte remediation** — the ~300 `as`-cast sites from the adversarial audit were **triaged**
  (see [`spec/AUDIT.md`](../../../spec/AUDIT.md) §8): two genuinely untrusted-input narrowing casts were
  fixed; the remainder are documented-faithful Scala fixed-width equivalents. The *safe-by-construction
  type system* itself is done: the sort-indexed `Par<S>` (compile-time Name/Proc) plus the load-bearing
  `Closed`/`WellScoped`/`BindsAtMostOnce` refinements
  ([`spec/RHO-CALCULUS.md`](../../../spec/RHO-CALCULUS.md)).
