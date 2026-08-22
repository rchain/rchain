# RChain — Rust rewrite

A faithful Rust rewrite of the [RChain](https://rchain.coop) node. The Rust implementation is a
Cargo workspace at the top level (one crate per original sbt module); the entire upstream Scala
fork is preserved for reference under [`legacy/`](legacy/).

## Documentation

The documentation is served as a book (`mdbook serve docs`). It is organized software-first:

- **Part I — Rholang & the ρ-calculus** ([`docs/src/rholang/`](docs/src/rholang/)) — the language:
  what it is, why it fits a blockchain, and from processes and names through object-capability smart
  contracts.
- **Part II — The ρ-calculus, formally** ([`docs/src/formal/`](docs/src/formal/)) — the grammar, the
  sorts, and the 19 laws, mapped to their machine-checked proofs.
- **Part III — The node** ([`docs/src/node/`](docs/src/node/)) — consensus, the tuple space, storage,
  and operation.
- **Part IV — Contributor / port** ([`docs/src/contributor/`](docs/src/contributor/)) — why Rust, and
  the per-module status.

The entry point for the book is [`docs/src/introduction.md`](docs/src/introduction.md); the
goal-indexed map for readers and AI agents is
[`docs/src/ai-entrypoint.md`](docs/src/ai-entrypoint.md).

## Why Rust

Two reasons drive the rewrite.

**Memory safety.** The Scala/JVM node leaked memory and paused on garbage collection — it shipped
JVM `Memory`/`GarbageCollector` diagnostics and needed `SBT_OPTS="-Xmx4g -Xss2m"` to run. Rust's
ownership model and lack of a tracing GC eliminate the leak and the stop-the-world pause by
construction.

**The calculus hierarchy.** Rust natively expresses the λ-calculus (closures), the π-calculus
(channels and `Send`/`Sync` name mobility), and the ρ-calculus (the reflective π-calculus: a name is
a quoted process, expressed here as the sortable `Par` value). The port's type discipline embeds ρ as
the base sort of a Calculus of Constructions, constructible and provable in Lean 4 and Coq.

The full argument — with a Rust → calculus → formalization correspondence table — is in
[docs/src/contributor/why-rust.md](docs/src/contributor/why-rust.md). The prose documentation is also
served as a book: `mdbook serve docs`.

## Governance

The rewrite is governed by [`AGENTS.md`](AGENTS.md) — the binding intent + formal specification —
and the machine-checked formalizations in [`spec/`](spec/) (the 19-law invariant inventory in
[`spec/INVENTORY.md`](spec/INVENTORY.md), plus the Lean/Coq tracks). The prime directive is a
**faithful implementation of the ρ-calculus**: the 19 laws are the oracle; the Scala node was the
port reference.

## Layout

Twelve crates mirror the original sbt modules, ported in dependency order. The per-crate status, the
layer map, the rewrite order, and the remaining work are consolidated in
[docs/src/contributor/architecture.md](docs/src/contributor/architecture.md).

## Build & test

```sh
cargo build
cargo test
```

Build and serve the documentation book:

```sh
mdbook serve docs   # or: mdbook build docs
```

## REPL

The `rnode` binary doubles as a thin gRPC client. Run the interactive rholang REPL against a node:

```sh
# Start a local standalone node (creates genesis), then in another terminal:
cargo run --release -p rchain-node --bin rnode -- run -s

# Interactive REPL (prompt `rholang $ `, history, tab-completion):
cargo run --release -p rchain-node --bin rnode -- repl

# …or point the client at a remote node (Repl is on the internal port 40402; Deploy is on 40401):
rnode --grpc-host <host> --grpc-port 40402 repl
```

Each term is parsed, normalized (an `Evaluating:` line is echoed on the node console), evaluated
against the node's isolated `eval-*` store, and printed as `Deployment cost:` + `Storage Contents:`.
`:q` quits. Evaluate files non-interactively with `rnode eval <file>...`.

## Docker multi-node network

A scripted pipeline builds the `rnode` image and boots a local **1–5 node** network on a Docker
bridge, then drives it from the CLI:

```sh
tools/docker-network.sh build            # build the rnode image
tools/docker-network.sh up 3             # bootstrap + 2 peers (any N in 1..5)
tools/docker-network.sh status           # docker ps for the network
tools/docker-network.sh cli bootstrap status
tools/docker-network.sh cli bootstrap repl
tools/docker-network.sh cli peer1 status
tools/docker-network.sh down             # stop (add -v to also drop the data volumes)
```

The bootstrap node runs standalone, creates genesis, and is bonded as a validator; peers bootstrap
from it over the TLS transport (`rnode://<id>@bootstrap?protocol=40400&discovery=40404`). The
`cli <node> <subcommand...>` helper runs the Rust client inside the network against that node.

The full operation guide (commands, ports, the genesis ceremony, and the multi-node topology) is in
[docs/src/node/operating.md](docs/src/node/operating.md).

## Where the Scala went

The upstream Scala fork — the sbt modules (`node/`, `sdk/`, `shared/`, `crypto/`, `models/`,
`rspace/`, `comm/`, `casper/`, `rholang/`, `block-storage/`, `regex/`, `graphz/`, `roscala/`,
`rosette/`, `rspace-bench/`), the sbt build (`build.sbt`, `project/`), configuration, CI, docs,
tooling, and data files — now lives under [`legacy/`](legacy/), with the original directory names.
