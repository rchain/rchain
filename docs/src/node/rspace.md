# The tuple space (RSpace)

Underneath rholang is **RSpace**, the **tuple space** that carries out all communication. Every
rholang send and receive is a transaction against this space, and the space's guarantees are what make
rholang's concurrency deterministic.

## Produce and consume

The tuple space has two operations:

- **produce** — put a message on a channel (a send). If a matching receiver is waiting, the two comm
  immediately; otherwise the message waits.
- **consume** — install a receiver on a channel (a receive). If a matching message is waiting, they
  comm; otherwise the receiver waits.

A **comm** is the single event that pairs a produce with a consume. The space is *asynchronous*: both
messages and receivers may wait, and the order in which they arrived does not matter.

## Joins

A rholang join — `for (x <- a; y <- b) { … }` — becomes a single **consume on multiple channels**. The
space matches it only when a message is present on *every* channel, and the match is **atomic**: the
join either sees all its messages or none. The join's key is the set of its channels, hashed in
**sorted order**, so `for (x <- a; y <- b)` and `for (y <- b; x <- a)` are the same join — that
order-independence is Law 7.

## Determinism

The space is deterministic (Laws 8, 11):

- When a produce has several possible matches, the space selects by a **sorted** ordering of the
  candidates — the produce refs are sorted, so the selection is reproducible.
- A comm event is **content-addressed**: the same produce/consume pair yields the same event id on
  every node.
- **Replay** recomputes comm events from a recorded trace, and the recomputed events must be a subset
  of the recorded ones — replay never invents a comm (Law 11). This is what lets a node *re-execute* a
  block and reach the same state.

## State as a Merkle radix trie

The space's state (every outstanding message and receiver) is stored in a **content-addressed Merkle
radix trie** (Law 10). Each node of the trie is hashed by its contents, so:

- the root hash **is** the state — two nodes with the same root have the same state;
- a node is **collision-free** (equal hashes imply equal contents);
- the **empty** state has a fixed, canonical empty root.

This is the substrate for the chain's state hashes: a block commits to a state root, and any two nodes
that replay the same deploys reach the same root.

## Merge and replay

When a block's deploys are applied, their effects on the space are **merged** (Law 9): state changes
compose associatively, and non-conflicting changes commute, so the order in which concurrent deploys
are merged does not affect the result. Replay is the reverse direction — re-deriving the same state
from a recorded trace — and is what makes the state auditable and re-verifiable.

> **Formal.** Joins, deterministic COMM, merge, Merkle structure, and replay are Laws 7–11. See
> [The 19 laws](../formal/the-19-laws.md) and the `RSpace` crate in
> [Architecture & port status](../contributor/architecture.md).
