# Rust-first native system contracts

This page records the **rust-first** replacement of the Scala-legacy genesis bootstrap: the
registry, Proof-of-Stake state, and REV vault are no longer rholang contracts (`Registry.rho`,
`Pos.rhox`, `RevVault.rho`, …) that re-implement a `TreeHashMap` trie *in interpreted rholang*.
They are now **native Rust state + system processes**. Scala serves as a *checklist* of required
behavior only (per the prime directive in [`AGENTS.md`](../AGENTS.md)); the fragile trie and the
registry-bootstrap echo are gone.

## Why

The blessed `Registry.rho` built a depth-4 keccak-256 nybble trie with 16-bit bitmasks, using peek
receives, persistent sends, list-channels (`@[node, *storeToken]`) and tuple-channels
(`@(map, "depth")`). Any interpreter bug made the registry **silently empty** — genesis "succeeded"
while `compute_bonds` returned zero. Native state makes that failure mode unrepresentable: the
bonds/registry/vault live in typed Rust maps, folded into the same content-addressed radix trie as
the tuple space.

## State model

The block state hash *is* the radix-trie root of the tuple space; there is no second state
component. Native state therefore enters the same trie, under dedicated prefixes:

| Prefix | Byte | Content | Leaf key |
|---|---|---|---|
| `PREFIX_REGISTRY` | `0x03` | registry `uri → Par` | `blake2b256(uri)` |
| `PREFIX_POS` | `0x04` | PoS bonds map | `blake2b256(b"pos:bonds")` |
| `PREFIX_VAULT` | `0x05` | vault `address → NonNegI64` | `blake2b256(address)` |

Leaves are the new `PersistedData::NativeLeaf(Vec<u8>)` (the previously-free 2-bit tag `3`). The
trie prefix disambiguates registry vs PoS vs vault, so a single leaf kind suffices.

The typed layer is `rholang/src/native_state.rs` (`NativeSystemState`), wrapping the byte-oriented
`rspace/src/native_store.rs` (`InMemNativeStore`):

- `InMemNativeStore` is a write-through overlay on a `NativeHistoryReader`: reads fall through to the
  persisted trie, writes buffer in an overlay + tombstone set, and `drain_changes` emits the
  `NativeStoreAction::{Put,Delete}`s folded into the next checkpoint via
  `HistoryRepository::checkpoint_with_native`.
- `NativeSystemState` exposes typed accessors — `bonds()`/`set_bonds()` (the active-validator set is
  *derived*: every bonded validator is active), `vault_balance()`/`set_vault_balance()`, and
  `registry_lookup()`/`registry_insert()`, with canonical byte encodings (sorted `BTreeMap`,
  fixed-width `Validator` + little-endian stake).

## Replay determinism

Native mutations are folded into the radix root, so replay reproduces them **only** if they are pure
functions of `(deploy, random_state)` — never wall-clock time or OS entropy. The system-deploy
operations (`pre_charge`/`refund`/`close_block`/`slash`) and the genesis install
(`compute_genesis(…, bonds)`) obey this; `replay_compute_state` re-installs the native bonds on the
genesis replay (`with_cost_accounting == false`) so the replayed root matches the play root. This is
asserted by `casper/tests/consensus.rs::empty_state_hash_fixed_matches_runtime` and
`genesis_deploy_replay_recomputes_state`.

## System processes

The native `rho:*` protocol is installed as ordinary system-process `Definition`s:

- `rho:registry:lookup` / `insertArbitrary` / `insertSigned:secp256k1` — backed by the native
  registry map.
- `rho:rchain:pos` — `getBonds` → `RhoMap`, `getActiveValidators` → `RhoSet`, `bond`/`withdraw`
  (validator lifecycle; simplified — see below) via a `remainder` install pattern.
- `rho:rchain:revVault` / `multiSigRevVault` — `getBalance` / `deposit` / `transfer` / `findOrCreate`
  over the vault balance map.

The `bond` (deduct the deployer's REV vault and add `(validator, stake)` to the bonds map, rejecting
already-bonded keys), `withdraw` (remove from the bonds map and refund the stake to the REV vault), and
vault `findOrCreate` (ensure a vault exists, creating a zero balance if absent) methods are implemented
natively, returning the `(Bool, Either)` result the PoS/vault contracts expect. **Deferred (simplified
balance-map model):** minimum/maximum-bond validation, the epoch/quarantine bookkeeping, reward
computation, the Coop slashing vault, and the vault **unforgeable-name capability** (the simplified
model keys vaults by REV address, so `findOrCreate` returns the address rather than a fresh
unforgeable).

`default_blessed_terms` now returns an empty list; the PoS bonds are installed natively in
`compute_genesis`, and `compute_bonds`/`get_active_validators` read the bonds leaf directly (no
rholang exploratory deploy). The pre-charge/refund/close-block/slash system deploys carry a
`NativeSystemDeployOp` and no longer route through `rho:registry:lookup` + the `Pos.rhox` contract.

## Cross-links

- [`spec/AUDIT.md`](AUDIT.md) §9 — the rust-first fragility audit that motivated this.
- [`spec/RHO-CALCULUS.md`](RHO-CALCULUS.md) — the ρ-calculus core this realizes.
- [`spec/TYPE-SYSTEM.md`](TYPE-SYSTEM.md) — the no-silent-partiality discipline.
