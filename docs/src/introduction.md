# RChain

RChain is a blockchain platform whose smart-contract language — **rholang** — is not a language
grafted onto a ledger, but a *concurrent, reflective process calculus*: the **ρ-calculus**. This book
is the software's documentation: what rholang is, why it is the way it is, how it runs, and how the
node that executes it works.

Rholang is a process-oriented language. A program is not a sequence of statements but a set of
**concurrent processes** that communicate by sending messages over **channels** (called *names*). The
defining idea of the ρ-calculus is **reflection**: a *name* is a *quoted process* (`@P`), and a process
can *evaluate* a name back into a process (`*x`). Because quoting and dereferencing code are built into
the language, programs can name other programs, pass code over channels, and build their own security
boundaries. That one fact is the foundation of everything rholang does well — deterministic
communication, compositional smart contracts, and unforgeable names.

## How to read this book

The book is layered, so it works for a first-time reader and for someone who has built on RChain for a
decade.

- **Part I — Rholang & the ρ-calculus** is the language. It starts with *why* rholang exists and
  builds up to object-capability smart contracts. Each chapter leads with intuition and ends with a
  pointer into the formal treatment.
- **Part II — The ρ-calculus, formally** is the precise semantics: the grammar, the sorts, and the
  **19 laws** that govern the language, each mapped to its machine-checked formalization.
- **Part III — The node** describes the software that executes rholang: the tuple space, the Merkle
  state, and the Casper consensus protocol.
- **Part IV — Contributor / port** is the engineering appendix: why the node is written in Rust, and
  the module-by-module status of the implementation.

## For AI agents

If you are an AI agent (or want the shortest path to a specific fact), start at
[Navigation for AI agents](ai-entrypoint.md) — a goal-indexed map of this book and the machine-checked
specification it links to. The authoritative formal specification lives outside this book, in the
[`spec/`](../../spec/) tree (the 19-law catalog and the Lean/Coq proofs); this book explains it, it does
not duplicate it.
