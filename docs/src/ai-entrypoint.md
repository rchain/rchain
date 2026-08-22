# Navigation for AI agents

This page is a goal-indexed map of the documentation and the machine-checked specification. Read it
first, then jump to the single page that answers your question. It mirrors the documentation map in
[`AGENTS.md`](../../AGENTS.md) but is organized by *goal* rather than by artifact.

The **authoritative formal specification** is the [`spec/`](../../spec/) tree — the 19-law catalog
([`spec/INVENTORY.md`](../../spec/INVENTORY.md)), the ρ-calculus core
([`spec/RHO-CALCULUS.md`](../../spec/RHO-CALCULUS.md)), and the ρ→CoC type discipline
([`spec/TYPE-SYSTEM.md`](../../spec/TYPE-SYSTEM.md)). This book explains those; it never duplicates them.

## By goal

| I want to… | Read |
|---|---|
| Understand *why* rholang exists and what makes it powerful | [Why rholang](rholang/why-rholang.md) |
| Learn the language from scratch | [Processes and names](rholang/processes-names.md) → [Sends and receives](rholang/sends-receives.md) |
| Understand `@` and `*` (quote/eval) | [Names are quoted processes](rholang/names-are-processes.md) |
| Understand what an **unforgeable name** is and why `new` matters | [Unforgeable names](rholang/unforgeable-names.md) |
| Understand pattern matching / spatial matching | [Patterns and matching](rholang/patterns-matching.md) |
| Build a secure contract (facets, revocation, sealer/unsealer, multisig) | [Object capabilities](rholang/object-capabilities.md), [Smart contracts](rholang/smart-contracts.md) |
| See the exact grammar and sorts | [Grammar and sorts](formal/grammar-sorts.md) |
| Map a language feature to its **law** and its proof | [The 19 laws](formal/the-19-laws.md) |
| Understand `≡` and `⟶` precisely | [Structural congruence and reduction](formal/congruence-reduction.md) |
| Understand the "no silent partiality" / totality guarantee | [Closedness and the Calculus of Constructions](formal/closedness-coc.md) |
| Understand consensus / finality | [Consensus (Casper)](node/consensus.md) |
| Understand the tuple space / storage | [The tuple space (RSpace)](node/rspace.md), [Storage](node/storage.md) |
| Understand the port (why Rust, module status) | [Part IV](contributor/why-rust.md) |
| Find the machine-checked proofs | [`spec/Rchain/`](../../spec/Rchain/) (Lean), [`spec/coq/`](../../spec/coq/) (Coq) |

## The invariant catalog, in one screen

RChain's behavior is pinned by **19 laws** (see [The 19 laws](formal/the-19-laws.md) and
[`spec/INVENTORY.md`](../../spec/INVENTORY.md)). They group as:

- **Rholang (Laws 1–6)** — canonicalization, α-equivalence, substitution, reduction, spatial matching,
  closedness.
- **RSpace (Laws 7–11)** — join commutativity, deterministic COMM, merge monoid, Merkle determinism,
  replay determinism.
- **Rosette (Laws 12–13)** — actor atomicity, reflection (orphaned; the VM is out of scope).
- **Casper (Laws 14–17)** — >2/3 finality, fringe/seen-set monotonicity, block validation, merge
  determinism.
- **Storage (Law 18)** — contiguous height map, order-independent fringe identity.
- **Crypto (Law 19)** — Blake2b256, splittable `Blake2b512Random`, signatures, Curve25519 (axiomatized).

## The formalization, in one screen

| Artifact | What it proves/states | Build |
|---|---|---|
| `spec/Rchain/*.lean` (Lean 4) | Law 1, `≡`/`⟶` core, `Closed`, totality fundamentals **proven**; Laws 3–5, 7–18 **stated**; crypto **axiomatized** | `cd spec && lake build` |
| `spec/coq/*.v` (Coq) | Laws 2–6 (substitution / α-equivalence metatheory) **stated** | `make -C spec/coq` |
| `spec/INVENTORY.md` | the 19-law catalog with source-of-truth + status | — |
| `spec/TYPE-SYSTEM.md` | the ρ→CoC type discipline (totality, refinements) | — |
