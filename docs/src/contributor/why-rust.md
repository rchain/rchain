# Why Rust

The RChain node executes Rholang, a concurrent message-passing language formally modeled by the
ρ-calculus — a *reflective, higher-order extension of the π-calculus*. The original node was written
in Scala on the JVM, with a C++ actor VM ([Rosette](../../../legacy/rosette/)) underneath. This
repository rewrites it in Rust.

Two reasons drive the rewrite.

## 1. Memory safety and deterministic resource use

The Scala/JVM node leaked memory and paused for garbage collection. These were not theoretical
concerns: the node shipped a `diagnostics` service that reported JVM `Memory`, `MemoryPool`, and
`GarbageCollector` metrics to its operators (see
[`legacy/docs/rnode-api/index.md`](../../../legacy/docs/rnode-api/index.md)), and the build needed an
enlarged heap and thread stack just to run (see [`legacy/DEVELOPER.md`](../../../legacy/DEVELOPER.md)):

```sh
export SBT_OPTS="-Xmx4g -Xss2m -Dsbt.supershell=false"
```

Rust eliminates this class of problem by construction. Ownership and the borrow checker make memory
leaks and use-after-free unrepresentable, with **no tracing garbage collector** — so there is no
stop-the-world pause and no heap pressure to tune away. Resource lifetime becomes a compile-time,
statically checked property rather than a runtime, best-effort one.

## 2. Rust natively expresses the calculus hierarchy

The second reason is deeper, and it is about what the node *is*, not just how it runs.

The ρ-calculus sits at the top of a hierarchy of process calculi:

- The **λ-calculus** is the calculus of substitution — of functions and application.
- The **π-calculus** adds concurrency and mobility: processes communicate over *channels*, and a
  channel is itself a value that can be passed over another channel.
- The **ρ-calculus** is the **reflective** π-calculus: a *name* is a *quoted process* (`@P`), and a
  process can *evaluate* a name back into a process (`*x`). Reflection — quoting and dereferencing
  code — is built into the calculus rather than bolted on.

Rust expresses each rung natively:

- **λ** — closures (`fn`, `Fn`/`FnMut`/`FnOnce`) are exactly λ-abstraction and application.
  Higher-order functions are pervasive, e.g. `SyncVar::update(f: impl FnOnce(A) -> A)` in
  [`shared/src/sync_var.rs`](../../../shared/src/sync_var.rs).
- **π** — Rust's concurrency primitives — `std::sync::mpsc`/`tokio::sync::mpsc` channels,
  `Arc` + `Mutex`/`Condvar`, and the `Send`/`Sync` marker traits — are channels and name passing. A
  cell such as `SyncVar`/`MaybeCell` is a degenerate channel.
- **ρ** — reflection. In the port the `Par` AST is a first-class, sortable, hashable value (the
  `Par`/`GUnforgeable` types in `models`), so a *name* **is** a quoted process — expressed as data,
  exactly as in the calculus.
- **Calculus of Constructions** — the dependent-type systems of Lean 4 and Coq. The port's type
  discipline embeds ρ as the base sort of a Calculus of Constructions and proves its fundamentals.

The correspondence table below maps each Rust construct to the calculus concept it expresses and to
the file in [`spec/`](../../../spec/) where that concept is formalized.

## Correspondence

| Rust construct | Calculus concept | Formal home |
|---|---|---|
| `fn` / `impl Fn` / closures | λ-abstraction and application | — |
| `std::sync::mpsc` / `tokio::sync::mpsc` channel | π-calculus channel (name) | — |
| `Arc` + `Mutex`/`Condvar`, `Send`/`Sync` | π name mobility (passing a channel) | — |
| `Par` / `GUnforgeable` value (sorted, hashed) | ρ quoted process / name | [`spec/Rchain/Par.lean`](../../../spec/Rchain/Par.lean), [`Sort.lean`](../../../spec/Rchain/Sort.lean) |
| `classify : Par → PSort`, `HasSort` | ρ base sort (process vs name) | [`spec/Rchain/Ty.lean`](../../../spec/Rchain/Ty.lean) |
| `Closed`, `Subst`, `Reduce` | α-equivalence, substitution, COMM (Laws 2–6) | [`spec/Rchain/Rho.lean`](../../../spec/Rchain/Rho.lean), [`Ty.lean`](../../../spec/Rchain/Ty.lean) |
| `TotalOn f := ∀ p, Closed p → Closed (f p)` | "no `.unwrap()`" totality | [`spec/TYPE-SYSTEM.md`](../../../spec/TYPE-SYSTEM.md) (F6) |
| Lean CIC / Coq CIC | Calculus of Constructions | [`spec/lakefile.toml`](../../../spec/lakefile.toml), [`spec/coq/_CoqProject`](../../../spec/coq/_CoqProject) |

## From calculus to proof

Because the hierarchy bottoms out in the Calculus of Constructions, the port's invariants can be
*constructed and proven* rather than merely asserted. This is what [`spec/`](../../../spec/) does:

- [`spec/TYPE-SYSTEM.md`](../../../spec/TYPE-SYSTEM.md) embeds the ρ-calculus as the base sort of a
  Calculus of Constructions and proves six fundamentals (F1–F6) in Lean 4 — sort classification is
  functional and decidable, structural congruence is an equivalence, substitution preserves sort,
  reduction preserves sort and closedness, canonicalization commutes with typing, and totality is
  compositional.
- [`spec/INVENTORY.md`](../../../spec/INVENTORY.md) is the **19-law invariant catalog** — one law per
  Rholang / RSpace / Rosette / Casper / Storage / Crypto invariant, each with a Scala source-of-truth
  pointer and a Lean/Coq formalization target.
- [`spec/Rchain/`](../../../spec/Rchain/) (Lean 4) and [`spec/coq/`](../../../spec/coq/) (Coq) hold the
  machine-checked definitions and theorems.

The claim "every fundamental property of expressing the node is contained within Rust *as-is*" is the
intuition; `spec/` is its machine-checked realization.

## Lineage

- Meredith & Radestock, *A Reflective Higher-Order Calculus* (2005) — the ρ-calculus.
- Meredith, *Higher Category Models of the π-Calculus* — the categorical semantics.
- The in-repo Rholang reference ([`legacy/rholang/reference_doc/`](../../../legacy/rholang/reference_doc/))
  documents the tuplespace model, quoting of processes into names, normalization (de Bruijn
  α-equivalence and the canonical `|` sort), and the ρ/λ/π relationship.

## A corollary: implement the calculus, don't reproduce the JVM

The port is complete; the node is now a *faithful implementation of the ρ-calculus*. The motivation is
memory safety and calculus-native expression, **not** a correctness repair of consensus behavior. The
binding constraint is stated in [`AGENTS.md`](../../../AGENTS.md): the 19 laws in
[`spec/INVENTORY.md`](../../../spec/INVENTORY.md) and the ρ→CoC type discipline in
[`spec/TYPE-SYSTEM.md`](../../../spec/TYPE-SYSTEM.md) are the oracle. Rust carries those invariants
structurally (refinement types, no silent partiality) rather than reproducing the JVM's patterns —
including its latent bugs.
